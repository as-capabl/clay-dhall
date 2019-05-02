{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}

module
    Clay.Core
where

import qualified Control.Monad.State.Strict as St
import Control.Applicative (liftA2)
import Control.Arrow (second)
import Data.Traversable (forM)
import Data.Functor.Contravariant ((>$))
import Data.Functor.Contravariant.Divisible (conquer, divide)
import Data.Void
import Data.Maybe (fromMaybe)

import Foreign.C.Types
import Foreign.C.String
import Foreign.Ptr
import Foreign.Storable
import Foreign.Marshal
import Foreign.StablePtr
import Data.Int
import Data.Word

import qualified Data.ByteString as B
import qualified Data.ByteString.Unsafe as B
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Vector as V
import qualified Data.Sequence as Seq

import qualified Dhall as Dh
import qualified Dhall.Core as DhC
import qualified Dhall.Parser as DhP
import qualified Dhall.TypeCheck as DhTC
import qualified Dhall.Map as DhMap

import Clay.Obj
import Clay.Type

type PeekType = Ptr () -> IO (Dh.InputType ())

type PokeFunc = Ptr () -> IO ()
type PokeType = Dh.Type PokeFunc

data CDhallTypeHolder = CDhallTypeHolder {
    thPeek :: PeekType,
    thPoke :: PokeType,
    thSizeOf :: Int
  }

holderStorable ::
    forall a b. (Dh.Interpret a, Dh.Inject a, Storable b) =>
    (a -> IO b) -> (b -> IO a) -> CDhallTypeHolder
holderStorable store fetch = CDhallTypeHolder {..}
  where
    storePoke x = \p -> store x >>= poke (castPtr p)
    fetchPeek p = peek (castPtr p) >>= fetch

    thPeek p = (>$ Dh.inject) <$> fetchPeek p
    thPoke = storePoke <$> Dh.auto
    thSizeOf = sizeOf (sizeDummy :: b)

allocCopyCS :: B.ByteString -> IO CString
allocCopyCS bs = B.unsafeUseAsCStringLen bs $ \(p, len) ->
  do
    cs <- cMalloc (fromIntegral $ len + 1)
    copyBytes cs p len
    poke (cs `plusPtr` len) (0 :: CChar)
    return cs

allocAndPokeArray :: Int -> V.Vector (Ptr () -> IO ()) -> Ptr () -> IO ()
allocAndPokeArray elemSize v pCDhallArray =
  do
    let len = V.length v
    p <- cMalloc (fromIntegral $ elemSize * len)
    V.imapM_ (\i write -> write $ p `plusPtr` (elemSize * i)) v
    poke (castPtr pCDhallArray) $ CDhallArray len p

peekAsArray :: DhC.Expr DhP.Src DhTC.X -> Int -> PeekType -> PeekType
peekAsArray declaredIn elemSize pt pCDhallArray =
  do
    CDhallArray len p <- peek (castPtr pCDhallArray)
    embededElems <- sequenceA $
      do
        i <- Seq.fromList [0 .. len - 1]
        return $ pt (p `plusPtr` (elemSize * i))
    let
        embedOut () = DhC.ListLit (Just declaredIn) (flip Dh.embed () <$> embededElems)
        declaredOut = DhC.App DhC.List declaredIn
    return $ Dh.InputType embedOut declaredOut

noPoke :: PokeFunc
noPoke _ = return ()

recordHolder :: CDhallRecordSpec -> IO CDhallTypeHolder
recordHolder CDhallRecordSpec{..} = 
  do
    flds <- forM [0 .. recordNumFields - 1] $ \i ->
      do
        CDhallFieldSpec {..} <- peekElemOff recordFields (fromIntegral i)
        txtName <- T.decodeUtf8 <$> B.unsafePackCString fieldName
        sp <- typeSpecBy fieldType
        let actOfs act p = act $ p `plusPtr` fromIntegral fieldOffset
            newPeek = actOfs $ \p -> Dh.inputFieldWith txtName <$> thPeek sp p
            newPoke = Dh.field txtName (actOfs <$> thPoke sp)
        return (newPeek, newPoke)

    return $ CDhallTypeHolder {
        thPeek = \p -> Dh.inputRecord <$> foldr (\f g -> liftA2 (divide $ \_ -> ((), ())) (f p) g) (return conquer) (fst <$> flds),
        thPoke = Dh.record (foldr (liftA2 $ \f g p -> f p >> g p) (pure (\_ -> return ())) (snd <$> flds)),
        thSizeOf = fromIntegral recordByteSize
      }

unionHolder :: CDhallUnionSpec -> IO CDhallTypeHolder
unionHolder CDhallUnionSpec{..} =
  do    
    l <- forM [0 .. unionNumItems - 1] $ \i ->
      do
        CDhallUItemSpec {..} <- peekElemOff unionItems (fromIntegral i)
        txtName <- T.decodeUtf8 <$> B.unsafePackCString uitemName
        sp <- typeSpecBy uitemType
        let uitem = pokeUnion i <$> thPoke sp
        return (txtName, (thPeek sp, uitem))

    let tp = DhMap.fromList l

    return $ CDhallTypeHolder {
        thPeek = \p ->
          do
            CDhallUnion {..} <- peekUnion (castPtr p)
            let i = fromIntegral unionIndex 
            let txtName = fst (l !! i)
                peeker = fst . snd $ l !! i
            pk <- peeker unionData
            return $ Dh.InputType {
                embed = \() -> DhC.UnionLit txtName (Dh.embed pk ()) (Dh.expected . snd <$> DhMap.delete txtName tp),
                declared = DhC.Union $ Dh.expected . snd <$> tp
              },
        thPoke = Dh.Type {
            extract = \(DhC.UnionLit k v _) ->
              do
                t <- DhMap.lookup k tp
                Dh.extract (snd t) v
                ,
            expected = DhC.Union $ Dh.expected . snd <$> tp
          },
        thSizeOf = unionOffset + fromIntegral unionByteSize
      }

typeSpecBy :: CDhallTypeSpec -> IO CDhallTypeHolder
typeSpecBy CDhallTypeSpec {..} =
    if
        | typeId == tBool -> return $ holderStorable
            ((\b -> return $ if b then 1 else 0) :: Bool -> IO CBool)
            (\cb -> return $ cb /= 0)
        | typeId == tNat -> return $ holderStorable
            (return . fromIntegral :: Dh.Natural -> IO CDhallWord)
            (return . fromIntegral)
        | typeId == tInt -> return $ holderStorable
            (return . fromIntegral :: Integer -> IO CDhallInt)
            (return . fromIntegral)
        | typeId == tString -> return $ holderStorable
            (allocCopyCS . T.encodeUtf8 :: T.Text -> IO CString)
            (\cs -> T.decodeUtf8 <$> B.unsafePackCString cs)
        | typeId == tDouble -> return $ holderStorable
            (return . realToFrac :: Double -> IO CDhallDouble)
            (return . realToFrac)
        | typeId == tArray ->
          do
            CDhallTypeHolder{..} <- typeSpecBy =<< peek (castPtr detail)
            return $ CDhallTypeHolder {
                thPeek = peekAsArray (Dh.expected thPoke) thSizeOf thPeek,
                thPoke = allocAndPokeArray thSizeOf <$> Dh.vector thPoke,
                thSizeOf = sizeOf (sizeDummy :: CDhallArray)
              }
        | typeId == tUnit ->
            return $ CDhallTypeHolder {
                thPeek = \_ -> return (Dh.inject),
                thPoke = noPoke <$ Dh.unit,
                thSizeOf = 1
              }
        | typeId == tOptional ->
          do
            sp <- typeSpecBy =<< peek (castPtr detail)
            return $ CDhallTypeHolder {
                thPeek = \p ->
                  do
                    CDhallUnion {..} <- peekUnion (castPtr p)
                    mx <- if unionIndex == coptSome
                        then Just <$> thPeek sp unionData
                        else return Nothing
                    let spExpected = Dh.expected $ thPoke sp
                    return $ Dh.InputType {
                        embed = \() -> DhC.OptionalLit spExpected (flip Dh.embed () <$> mx),
                        declared = DhC.App DhC.Optional spExpected
                      }
                    ,
                thPoke = maybe (pokeUnion coptNone noPoke) (pokeUnion coptSome) <$> Dh.maybe (thPoke sp),
                thSizeOf = thSizeOf sp + unionOffset
              }
        | typeId == tRecord ->
            peek (castPtr detail) >>= recordHolder
        | typeId == tUnion ->
            peek (castPtr detail) >>= unionHolder
        | typeId == tFunction ->
          do
            argSpec <- typeSpecBy =<< peek (funcArgSpec detail)
            resultSpec <- typeSpecBy =<< peek (funcResultSpec detail)

            return $ CDhallTypeHolder {
                thPeek = error "Function input is not allowed",
                thPoke = Dh.Type {
                    extract = \e -> Just $ \pTEFunc ->
                      do
                        s <- newStablePtr $ teFuncToObj $ \pArg pDest ->
                          do
                            argInput <- thPeek argSpec pArg
                            case Dh.extract (thPoke resultSpec) (DhC.normalize (DhC.App e (Dh.embed argInput ())))
                              of
                                Just o -> o pDest
                                Nothing -> error "Type mismatch at typeId == tFunction"
                        poke (castPtr pTEFunc) s        
                        ,
                    expected = DhC.Pi "_" (Dh.expected (thPoke argSpec)) (Dh.expected (thPoke resultSpec))
                  },
                thSizeOf = sizeOf (sizeDummy :: Ptr Obj)
              }

typeSpecByN :: CDhallInt -> Ptr CDhallTypeSpec -> IO (V.Vector CDhallTypeHolder)
typeSpecByN n p = V.generateM (fromIntegral n) $ \i -> peekElemOff p i >>= typeSpecBy


asHolderType :: Ptr CDhallTypedPtr -> IO (Dh.Type (IO ()))
asHolderType p =
  do
    CDhallTypedPtr{..} <- peekTypedPtr p
    spec <- peek tptrSpec
    CDhallTypeHolder{..} <- typeSpecBy spec
    return $ ($tptrPtr) <$> thPoke
