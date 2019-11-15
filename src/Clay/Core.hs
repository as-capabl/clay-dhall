{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}

module
    Clay.Core
where

import qualified Control.Monad.State.Strict as St
import Control.Monad.Reader
import Control.Applicative (liftA2)
import Control.Arrow (second)
import Data.Traversable (forM)
import Data.Functor.Contravariant
import Data.Functor.Contravariant.Divisible
import Data.Functor.Product
import Data.Functor.Const
import Data.Void
import Data.Maybe (fromMaybe)
import Data.Either.Validation

import Foreign.C.Types
import Foreign.C.String
import Foreign.Ptr
import Foreign.Storable
import Foreign.Marshal
import Foreign.StablePtr
import Data.Int
import Data.Word
import Unsafe.Coerce

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

--
-- Peek / Poke Types
--
data PeekType' f = forall a. PeekType (ReaderT (Ptr ()) IO a) (f a)
type PeekType = PeekType' Dh.InputType

type PokeFunc = Ptr () -> IO ()
type PokeType = Dh.Type PokeFunc

runPeekType :: PeekType -> Ptr () -> IO (Dh.InputType ())
runPeekType (PeekType rd ity) = runReaderT $
  do
    x <- rd
    return $ const x >$< ity

hoistPeekType :: (forall a. f a -> g a) -> PeekType' f -> PeekType' g
hoistPeekType p (PeekType f x) = PeekType f (p x)

instance Divisible f => Semigroup (PeekType' f)
  where
    PeekType f1 x1 <> PeekType f2 x2 = PeekType (liftA2 (,) f1 f2) (divided x1 x2)

instance Divisible f => Monoid (PeekType' f)
  where
    mempty = PeekType (return ()) conquer

--
-- Type Spec
--
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
    fetchPeek = ask >>= \p -> lift (peek (castPtr p) >>= fetch)

    thPeek = PeekType fetchPeek Dh.inject
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

peekAsArray :: Int -> PeekType -> PeekType
peekAsArray elemSize (PeekType fElem ityElem) = PeekType f (injectSequence ityElem)
  where
    f = ReaderT $ \pCDhallArray ->
      do
        CDhallArray len p <- peek (castPtr pCDhallArray)
        St.evalStateT `flip` (0 :: Int) $
            Seq.replicateA len $ St.StateT $ \i ->
              do
                y <- runReaderT fElem (p `plusPtr` (elemSize * i))
                return (y, i+1)

noPoke :: PokeFunc
noPoke _ = return ()

optionalHolder :: CDhallTypeHolder -> CDhallTypeHolder
optionalHolder sp = CDhallTypeHolder {
    thPeek = thPeekThis (thPeek sp),
    thPoke = maybe (pokeUnion coptNone noPoke) (pokeUnion coptSome) <$> Dh.maybe (thPoke sp),
    thSizeOf = thSizeOf sp + unionOffset
  }
  where
    thPeekThis (PeekType fElem ityElem) = PeekType (f fElem) (injectMaybe ityElem)
    f fElem = ReaderT $ \p ->
      do
        CDhallUnion {..} <- peekUnion (castPtr p)
        if unionIndex == coptSome
            then Just <$> runReaderT fElem unionData
            else return Nothing

recordHolder :: CDhallRecordSpec -> IO CDhallTypeHolder
recordHolder CDhallRecordSpec{..} =
  do
    flds <- forM [0 .. recordNumFields - 1] $ \i ->
      do
        CDhallFieldSpec {..} <- peekElemOff recordFields (fromIntegral i)
        txtName <- T.decodeUtf8 <$> B.unsafePackCString fieldName
        sp <- typeSpecBy fieldType
        let actOfs act = ReaderT $ \p -> act $ p `plusPtr` fromIntegral fieldOffset
            newPeek = case hoistPeekType (Dh.inputFieldWith txtName) $ thPeek sp
              of
                PeekType f x ->
                    PeekType (local (`plusPtr` fromIntegral fieldOffset) f) x
            newPoke = Dh.field txtName (actOfs <$> thPoke sp)
        return (newPeek, newPoke)

    return $ CDhallTypeHolder {
        thPeek = hoistPeekType Dh.inputRecord $ mconcat (fst <$> flds),
        thPoke = Dh.record $ runReaderT <$> foldr (liftA2 (>>)) (pure $ return ()) (snd <$> flds),
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
        let newPeek = hoistPeekType (Dh.inputConstructorWith txtName) $ thPeek sp
            newPoke = Dh.constructor txtName $ pokeUnion i <$> thPoke sp
        return (newPeek, newPoke)
    let readPk = ReaderT $ \p ->
          do
            CDhallUnion {..} <- peekUnion (castPtr p)
            let i = fromIntegral unionIndex
            x <- case fst (l !! i)
              of
                PeekType rd _ -> unsafeCoerce <$> runReaderT rd unionData
            return (i, x :: ())
        coerceInputPk (i, x) = if i == 0 then Left (unsafeCoerce x) else Right (i-1, x)
        combInputPk (PeekType _ ity) rest = contramap coerceInputPk (ity Dh.>|< rest)
        termInputPk = const (error "imposible by unionHolder") >$< unionInputVoid
        inputPk = Dh.inputUnion $ foldr combInputPk termInputPk $ fst <$> l

    return $ CDhallTypeHolder {
        thPeek = PeekType readPk inputPk,
        thPoke = Dh.union (foldMap snd l),
        thSizeOf = unionOffset + fromIntegral unionByteSize
      }

unionInputVoid :: Dh.UnionInputType Void
unionInputVoid = Dh.UnionInputType $ Pair (Const mempty) (Op absurd)

injectMaybe :: Dh.InputType a -> Dh.InputType (Maybe a)
injectMaybe (Dh.InputType embedIn declaredIn) =
    Dh.InputType embedOut declaredOut
  where
    embedOut (Just x) = DhC.Some (embedIn x)
    embedOut Nothing = DhC.App DhC.None declaredIn
    declaredOut = DhC.App DhC.Optional declaredIn

injectSequence :: Dh.InputType a -> Dh.InputType (Seq.Seq a)
injectSequence (Dh.InputType embedIn declaredIn) =
    Dh.InputType embedOut declaredOut
  where
    embedOut xs = DhC.ListLit listType (fmap embedIn xs)
      where
        listType
            | null xs   = Just (DhC.App DhC.List declaredIn)
            | otherwise = Nothing
    declaredOut = DhC.App DhC.List declaredIn

typeSpecBy :: CDhallTypeSpec -> IO CDhallTypeHolder
typeSpecBy CDhallTypeSpec {..}
    | typeId == tBool = return $ holderStorable
        ((\b -> return $ if b then 1 else 0) :: Bool -> IO CBool)
        (\cb -> return $ cb /= 0)
    | typeId == tNat = return $ holderStorable
        (return . fromIntegral :: Dh.Natural -> IO CDhallWord)
        (return . fromIntegral)
    | typeId == tInt = return $ holderStorable
        (return . fromIntegral :: Integer -> IO CDhallInt)
        (return . fromIntegral)
    | typeId == tString = return $ holderStorable
        (allocCopyCS . T.encodeUtf8 :: T.Text -> IO CString)
        (\cs -> T.decodeUtf8 <$> B.unsafePackCString cs)
    | typeId == tDouble = return $ holderStorable
        (return . realToFrac :: Double -> IO CDhallDouble)
        (return . realToFrac)
    | typeId == tArray =
      do
        CDhallTypeHolder{..} <- typeSpecBy =<< peek (castPtr detail)
        return $ CDhallTypeHolder {
            thPeek = peekAsArray thSizeOf thPeek,
            thPoke = allocAndPokeArray thSizeOf <$> Dh.vector thPoke,
            thSizeOf = sizeOf (sizeDummy :: CDhallArray)
          }
    | typeId == tUnit =
        return $ CDhallTypeHolder {
            thPeek = PeekType (return ()) Dh.inject,
            thPoke = noPoke <$ Dh.unit,
            thSizeOf = 1
          }
    | typeId == tOptional =
      do
        sp <- typeSpecBy =<< peek (castPtr detail)
        return $ optionalHolder sp
    | typeId == tRecord =
        peek (castPtr detail) >>= recordHolder
    | typeId == tUnion =
        peek (castPtr detail) >>= unionHolder
    | typeId == tFunction =
      do
        argSpec <- typeSpecBy =<< peek (funcArgSpec detail)
        resultSpec <- typeSpecBy =<< peek (funcResultSpec detail)

        return $ CDhallTypeHolder {
            thPeek = error "Function input is not allowed",
            thPoke = Dh.Type {
                extract = \e -> Success $ \pTEFunc ->
                  do
                    s <- newStablePtr $ teFuncToObj $ \pArg pDest ->
                      do
                        argInput <- runPeekType (thPeek argSpec) pArg
                        case Dh.extract (thPoke resultSpec) (DhC.normalize (DhC.App e (Dh.embed argInput ())))
                          of
                            Success o -> o pDest
                            Failure src -> error ("Type mismatch at typeId == tFunction " ++ show src)
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
