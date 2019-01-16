{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}

module
    Clay.Type
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

import qualified Dhall as Dh
import qualified Dhall.Core as DhC
import qualified Dhall.Parser as DhP
import qualified Dhall.TypeCheck as DhTC
import qualified Dhall.Map as DhMap

import Clay.Obj

#include "clay-dhall/type.h"

foreign import ccall "malloc" cMalloc :: CSize -> IO (Ptr a)
foreign import ccall "free" cFree :: Ptr a -> IO ()

type CDhallInt = #{type cdhall_int}

data CDhallArray = CDhallArray {
    arraySize :: Int,
    arrayData :: Ptr ()
  }

data CDhallTypedPtr = CDhallTypedPtr {
    tptrSpec :: Ptr CDhallTypeHolder,
    tptrPtr :: Ptr ()
  }

peekTypedPtr :: Ptr (CDhallTypedPtr) -> IO CDhallTypedPtr
peekTypedPtr p =
  do
    let tptrSpec = p `plusPtr` #{offset cdhall_typed_ptr, spec}
    tptrPtr <- #{peek cdhall_typed_ptr, ptr} p  
    return CDhallTypedPtr{..}

instance Storable CDhallArray
  where
    peek p =
      do
        arraySize <- #{peek cdhall_array, size} p
        arrayData <- #{peek cdhall_array, elem} p  
        return CDhallArray{..}
    poke p CDhallArray {..} =
      do
        #{poke cdhall_array, size} p arraySize
        #{poke cdhall_array, elem} p arrayData
    sizeOf _ = #{size cdhall_array}
    alignment _ = #{alignment cdhall_array}

--
-- Type id and handlers
--

type CDhallTypeId = #{type cdhall_type}

#{enum CDhallTypeId,,
    tBool = CDHALL_TYPE_BOOL, 
    tNat = CDHALL_TYPE_NAT,
    tInt = CDHALL_TYPE_INT,
    tString = CDHALL_TYPE_STRING,
    tDouble = CDHALL_TYPE_DOUBLE,
    tArray = CDHALL_TYPE_ARRAY,
    tUnit = CDHALL_TYPE_UNIT,
    tOptional = CDHALL_TYPE_OPTIONAL,
    tRecord = CDHALL_TYPE_RECORD,
    tUnion = CDHALL_TYPE_UNION,
    tFunApp = CDHALL_TYPE_FUNAPP
}

#{enum CDhallInt,,
    coptNone = CDHALL_OPTIONAL_NONE,
    coptSome = CDHALL_OPTIONAL_SOME}

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
    thSizeOf = sizeOf (undefined :: b)

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

asUnionItem :: CDhallInt -> PokeFunc -> PokeFunc
asUnionItem i oldPoke = \p ->
  do
    #{poke cdhall_union, index} (castPtr p) i 
    oldPoke (p `plusPtr` #{offset cdhall_union, data})

noPoke :: PokeFunc
noPoke _ = return ()

typeSpecBy :: Ptr CDhallTypeHolder -> IO CDhallTypeHolder
typeSpecBy p =
  do
    typeId <- #{peek cdhall_type_spec, typeId} p :: IO CDhallTypeId
    detail <- #{peek cdhall_type_spec, detail} p :: IO (Ptr ())
    if
        | typeId == tBool -> return $ holderStorable
            ((\b -> return $ if b then 1 else 0) :: Bool -> IO CBool)
            (\cb -> return $ cb /= 0)
        | typeId == tNat -> return $ holderStorable
            (return . fromIntegral :: Dh.Natural -> IO #{type cdhall_uint})
            (return . fromIntegral)
        | typeId == tInt -> return $ holderStorable
            (return . fromIntegral :: Integer -> IO #{type cdhall_int})
            (return . fromIntegral)
        | typeId == tString -> return $ holderStorable
            (allocCopyCS . T.encodeUtf8 :: T.Text -> IO CString)
            (\cs -> T.decodeUtf8 <$> B.unsafePackCString cs)
        | typeId == tDouble -> return $ holderStorable
            (return . realToFrac :: Double -> IO #{type double})
            (return . realToFrac)
        | typeId == tArray ->
          do
            CDhallTypeHolder{..} <- typeSpecBy (castPtr detail)
            return $ CDhallTypeHolder {
                thPeek = undefined,
                thPoke = allocAndPokeArray thSizeOf <$> Dh.vector thPoke,
                thSizeOf = sizeOf (undefined :: CDhallArray)
              }
        | typeId == tUnit ->
            return $ CDhallTypeHolder {
                thPeek = undefined,
                thPoke = noPoke <$ Dh.unit,
                thSizeOf = 1
              }
        | typeId == tOptional ->
          do
            sp <- typeSpecBy $ castPtr detail
            return $ CDhallTypeHolder {
                thPeek = undefined,
                thPoke = maybe (asUnionItem coptNone noPoke) (asUnionItem coptSome) <$> Dh.maybe (thPoke sp),
                thSizeOf = thSizeOf sp + #{offset cdhall_union, data}
              }
        | typeId == tRecord ->
          do
            numFields <- #{peek cdhall_record_spec, numFields} detail :: IO #{type cdhall_int}
            byteSize <- #{peek cdhall_record_spec, byteSize} detail :: IO CSize
            pFields <- #{peek cdhall_record_spec, pFields} detail :: IO (Ptr ())

            flds <- forM [0 .. numFields - 1] $ \i ->
              do
                let pFld = pFields `plusPtr` fromIntegral (#{size cdhall_field_spec} * i)

                csName <- #{peek cdhall_field_spec, name} pFld :: IO CString
                txtName <- T.decodeUtf8 <$> B.unsafePackCString csName

                ofs <- #{peek cdhall_field_spec, offset} pFld :: IO CSize

                sp <- typeSpecBy (castPtr pFld `plusPtr` #{offset cdhall_field_spec, type})

                let actOfs act p = act $ p `plusPtr` fromIntegral ofs
                    newPeek = actOfs $ \p -> Dh.inputFieldWith txtName <$> thPeek sp p
                    newPoke = Dh.field txtName (actOfs <$> thPoke sp)
                return (newPeek, newPoke)

            return $ CDhallTypeHolder {
                thPeek = \p -> Dh.inputRecord <$> foldr (\f g -> liftA2 (divide $ \_ -> ((), ())) (f p) g) (return conquer) (fst <$> flds),
                thPoke = Dh.record (foldr (liftA2 $ \f g p -> f p >> g p) (pure (\_ -> return ())) (snd <$> flds)),
                thSizeOf = fromIntegral byteSize
              }
        | typeId == tUnion ->
          do
            numItems <- #{peek cdhall_union_spec, numItems} detail :: IO #{type cdhall_int}
            byteSize <- #{peek cdhall_union_spec, byteSize} detail :: IO CSize
            pItems <- #{peek cdhall_union_spec, pItems} detail :: IO (Ptr ())
            l <- forM [0 .. numItems - 1] $ \i ->
              do
                let pItm = pItems `plusPtr` fromIntegral (#{size cdhall_uitem_spec} * fromIntegral i)

                csName <- #{peek cdhall_uitem_spec, name} pItm :: IO CString
                txtName <- T.decodeUtf8 <$> B.unsafePackCString csName

                sp <- typeSpecBy (castPtr pItm `plusPtr` #{offset cdhall_uitem_spec, type})
                let uitem = asUnionItem i <$> thPoke sp
                return (txtName, uitem)

            let tp = DhMap.fromList l

            return $ CDhallTypeHolder {
                thPeek = undefined,
                thPoke = Dh.Type {
                    extract = \(DhC.UnionLit k v _) ->
                      do
                        t <- DhMap.lookup k tp
                        Dh.extract t v
                        ,
                    expected = DhC.Union $ Dh.expected <$> tp
                  },
                thSizeOf = #{offset cdhall_union, data} + fromIntegral byteSize
              }

        | typeId == tFunApp ->
          do
            argSpec <- typeSpecBy (castPtr detail `plusPtr` #{offset cdhall_funapp, argSpec})
            resultSpec <- typeSpecBy (castPtr detail `plusPtr` #{offset cdhall_funapp, resultSpec})

            return $ CDhallTypeHolder {
                thPeek = undefined,
                thPoke = Dh.Type {
                    extract = \e -> Just $ \pTEFunc ->
                      do
                        s <- newStablePtr $ teFuncToObj $ \pArg pDest ->
                          do
                            argInput <- thPeek argSpec pArg
                            case Dh.extract (thPoke resultSpec) (DhC.normalize (DhC.App e (Dh.embed argInput ())))
                              of
                                Just o -> o pDest
                                Nothing -> error "Type mismatch at typeId == tFunApp"
                        poke (castPtr pTEFunc) s        
                        ,
                    expected = DhC.Pi "_" (Dh.expected (thPoke argSpec)) (Dh.expected (thPoke resultSpec))
                  },
                thSizeOf = sizeOf (undefined :: Ptr Obj)
              }

typeSpecByN :: CDhallInt -> Ptr CDhallTypeHolder -> IO (V.Vector CDhallTypeHolder)
typeSpecByN n p = V.generateM (fromIntegral n) $ \i ->
    typeSpecBy $ p `plusPtr` (#{size cdhall_type_spec} * i)


asHolderType :: Ptr CDhallTypedPtr -> IO (Dh.Type (IO ()))
asHolderType p =
  do
    CDhallTypedPtr{..} <- peekTypedPtr p
    CDhallTypeHolder{..} <- typeSpecBy tptrSpec
    return $ ($tptrPtr) <$> thPoke

