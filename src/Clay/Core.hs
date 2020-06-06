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
type PeekType = PeekType' Dh.Encoder

type PokeFunc = Ptr () -> IO ()
type PokeType = Dh.Decoder PokeFunc

runPeekType :: PeekType -> Ptr () -> IO (Dh.Encoder ())
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
-- Accessor
--
data Accessor = Accessor {
    thPeek :: PeekType,
    thPoke :: PokeType,
    thSizeOf :: Int
  }

accStorable ::
    forall a b. (Dh.Interpret a, Dh.Inject a, Storable b) =>
    (a -> IO b) -> (b -> IO a) -> Accessor
accStorable store fetch = Accessor {..}
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

accOptional :: Accessor -> Accessor
accOptional sp = Accessor {
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

accRecord :: CDhallRecordSpec -> IO Accessor
accRecord CDhallRecordSpec{..} =
  do
    flds <- forM [0 .. recordNumFields - 1] $ \i ->
      do
        CDhallFieldSpec {..} <- peekElemOff recordFields (fromIntegral i)
        txtName <- T.decodeUtf8 <$> B.unsafePackCString fieldName
        sp <- fromTypeSpec fieldType
        let actOfs act = ReaderT $ \p -> act $ p `plusPtr` fromIntegral fieldOffset
            newPeek = case hoistPeekType (Dh.encodeFieldWith txtName) $ thPeek sp
              of
                PeekType f x ->
                    PeekType (local (`plusPtr` fromIntegral fieldOffset) f) x
            newPoke = Dh.field txtName (actOfs <$> thPoke sp)
        return (newPeek, newPoke)

    return $ Accessor {
        thPeek = hoistPeekType Dh.recordEncoder $ mconcat (fst <$> flds),
        thPoke = Dh.record $ runReaderT <$> foldr (liftA2 (>>)) (pure $ return ()) (snd <$> flds),
        thSizeOf = fromIntegral recordByteSize
      }

accUnion :: CDhallUnionSpec -> IO Accessor
accUnion CDhallUnionSpec{..} =
  do
    l <- forM [0 .. unionNumItems - 1] $ \i ->
      do
        CDhallUItemSpec {..} <- peekElemOff unionItems (fromIntegral i)
        txtName <- T.decodeUtf8 <$> B.unsafePackCString uitemName
        sp <- fromTypeSpec uitemType
        let newPeek = hoistPeekType (Dh.encodeConstructorWith txtName) $ thPeek sp
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
        termInputPk = const (error "imposible by accUnion") >$< unionInputVoid
        inputPk = Dh.unionEncoder $ foldr combInputPk termInputPk $ fst <$> l

    return $ Accessor {
        thPeek = PeekType readPk inputPk,
        thPoke = Dh.union (foldMap snd l),
        thSizeOf = unionOffset + fromIntegral unionByteSize
      }

unionInputVoid :: Dh.UnionEncoder Void
unionInputVoid = Dh.UnionEncoder $ Pair (Const mempty) (Op absurd)

injectMaybe :: Dh.Encoder a -> Dh.Encoder (Maybe a)
injectMaybe (Dh.Encoder embedIn declaredIn) =
    Dh.Encoder embedOut declaredOut
  where
    embedOut (Just x) = DhC.Some (embedIn x)
    embedOut Nothing = DhC.App DhC.None declaredIn
    declaredOut = DhC.App DhC.Optional declaredIn

injectSequence :: Dh.Encoder a -> Dh.Encoder (Seq.Seq a)
injectSequence (Dh.Encoder embedIn declaredIn) =
    Dh.Encoder embedOut declaredOut
  where
    embedOut xs = DhC.ListLit listType (fmap embedIn xs)
      where
        listType
            | null xs   = Just (DhC.App DhC.List declaredIn)
            | otherwise = Nothing
    declaredOut = DhC.App DhC.List declaredIn

fromTypeSpec :: CDhallTypeSpec -> IO Accessor
fromTypeSpec CDhallTypeSpec {..}
    | typeId == tBool = return $ accStorable
        ((\b -> return $ if b then 1 else 0) :: Bool -> IO CBool)
        (\cb -> return $ cb /= 0)
    | typeId == tNat = return $ accStorable
        (return . fromIntegral :: Dh.Natural -> IO CDhallWord)
        (return . fromIntegral)
    | typeId == tInt = return $ accStorable
        (return . fromIntegral :: Integer -> IO CDhallInt)
        (return . fromIntegral)
    | typeId == tString = return $ accStorable
        (allocCopyCS . T.encodeUtf8 :: T.Text -> IO CString)
        (\cs -> T.decodeUtf8 <$> B.unsafePackCString cs)
    | typeId == tDouble = return $ accStorable
        (return . realToFrac :: Double -> IO CDhallDouble)
        (return . realToFrac)
    | typeId == tArray =
      do
        Accessor{..} <- fromTypeSpec =<< peek (castPtr detail)
        return $ Accessor {
            thPeek = peekAsArray thSizeOf thPeek,
            thPoke = allocAndPokeArray thSizeOf <$> Dh.vector thPoke,
            thSizeOf = sizeOf (sizeDummy :: CDhallArray)
          }
    | typeId == tUnit =
        return $ Accessor {
            thPeek = PeekType (return ()) Dh.inject,
            thPoke = noPoke <$ Dh.unit,
            thSizeOf = 1
          }
    | typeId == tOptional =
      do
        sp <- fromTypeSpec =<< peek (castPtr detail)
        return $ accOptional sp
    | typeId == tRecord =
        peek (castPtr detail) >>= accRecord
    | typeId == tUnion =
        peek (castPtr detail) >>= accUnion
    | typeId == tFunction =
      do
        argSpec <- fromTypeSpec =<< peek (funcArgSpec detail)
        resultSpec <- fromTypeSpec =<< peek (funcResultSpec detail)

        return $ Accessor {
            thPeek = error "Function input is not allowed",
            thPoke = case thPeek argSpec
              of
                PeekType rd fx -> 
                    let dec = Dh.function fx (thPoke resultSpec)
                        appRd f = \pF -> newStablePtr (teFuncToObj $ tef f) >>= poke (castPtr pF)
                        tef f pArg pDest =
                          do
                            x <- runReaderT rd pArg
                            f x pDest
                      in
                        appRd <$> dec,
            thSizeOf = sizeOf (sizeDummy :: Ptr Obj)
          }

fromTypeSpecN :: CDhallInt -> Ptr CDhallTypeSpec -> IO (V.Vector Accessor)
fromTypeSpecN n p = V.generateM (fromIntegral n) $ \i -> peekElemOff p i >>= fromTypeSpec


runTPtr :: Ptr CDhallTypedPtr -> IO (Dh.Decoder (IO ()))
runTPtr p =
  do
    CDhallTypedPtr{..} <- peekTypedPtr p
    spec <- peek tptrSpec
    Accessor{..} <- fromTypeSpec spec
    return $ ($tptrPtr) <$> thPoke
