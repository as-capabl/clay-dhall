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

import Foreign.C.Types
import Foreign.C.String
import Foreign.Ptr
import Foreign.Storable
import Foreign.Marshal
import Foreign.StablePtr
import Data.Int
import Data.Word


import Clay.Obj

#include "clay-dhall/type.h"

foreign import ccall "malloc" cMalloc :: CSize -> IO (Ptr a)
foreign import ccall "free" cFree :: Ptr a -> IO ()

sizeDummy :: a
sizeDummy = error "Size Dummy"

type CDhallInt = #{type cdhall_int}
type CDhallWord = #{type cdhall_uint}
type CDhallDouble = #{type double}


data CDhallTypedPtr = CDhallTypedPtr {
    tptrSpec :: Ptr CDhallTypeSpec,
    tptrPtr :: Ptr ()
  }

peekTypedPtr :: Ptr (CDhallTypedPtr) -> IO CDhallTypedPtr
peekTypedPtr p =
  do
    let tptrSpec = p `plusPtr` #{offset cdhall_typed_ptr, spec}
    tptrPtr <- #{peek cdhall_typed_ptr, ptr} p  
    return CDhallTypedPtr{..}

data CDhallArray = CDhallArray {
    arraySize :: Int,
    arrayData :: Ptr ()
  }

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

data CDhallTypeSpec = CDhallTypeSpec {
    typeId :: CDhallTypeId,
    detail :: Ptr ()
  }

instance Storable CDhallTypeSpec
  where
    peek p =
      do
        typeId <- #{peek cdhall_type_spec, typeId} p
        detail <- #{peek cdhall_type_spec, detail} p  
        return CDhallTypeSpec {..}
    poke p CDhallTypeSpec{..} =
      do
        #{poke cdhall_type_spec, typeId} p typeId
        #{poke cdhall_type_spec, detail} p detail
    sizeOf _ = #{size cdhall_type_spec}
    alignment _ = #{alignment cdhall_array}  

data CDhallRecordSpec = CDhallRecordSpec {
    recordNumFields :: CDhallInt,
    recordByteSize :: CSize,
    recordFields :: Ptr CDhallFieldSpec
  }
instance Storable CDhallRecordSpec
  where
    peek p =
      do
        recordNumFields <- #{peek cdhall_record_spec, numFields} p
        recordByteSize <- #{peek cdhall_record_spec, byteSize} p
        recordFields <- #{peek cdhall_record_spec, pFields} p
        return CDhallRecordSpec {..}
    poke = undefined
    sizeOf _ = #{size cdhall_record_spec}
    alignment _ = #{alignment cdhall_record_spec}


data CDhallUnionSpec = CDhallUnionSpec {
    unionNumItems :: CDhallInt,
    unionByteSize :: CSize,
    unionItems :: Ptr CDhallUItemSpec
  }

instance Storable CDhallUnionSpec
  where
    peek p =
      do
        unionNumItems <- #{peek cdhall_union_spec, numItems} p
        unionByteSize <- #{peek cdhall_union_spec, byteSize} p
        unionItems <- #{peek cdhall_union_spec, pItems} p
        return CDhallUnionSpec {..}
    poke = undefined
    sizeOf _ = #{size cdhall_union_spec}
    alignment _ = #{alignment cdhall_union_spec}

data CDhallUnion = CDhallUnion {
    unionIndex :: CDhallInt,
    unionData :: Ptr ()
  } -- Size varying struct. Not storable.

peekUnion :: Ptr () -> IO CDhallUnion
peekUnion p =
  do
    unionIndex <- #{peek cdhall_union, index} p
    let unionData = p `plusPtr` #{offset cdhall_union, data}
    return CDhallUnion {..}

pokeUnion :: CDhallInt -> (Ptr () -> IO ()) -> Ptr () -> IO ()
pokeUnion i oldPoke p = 
  do
    #{poke cdhall_union, index} (castPtr p) i 
    oldPoke (p `plusPtr` unionOffset)
  

unionOffset :: Int
unionOffset = #{offset cdhall_union, data}

funcArgSpec :: Ptr () -> Ptr CDhallTypeSpec
funcArgSpec p = p `plusPtr` #{offset cdhall_func_spec, argSpec}

funcResultSpec :: Ptr () -> Ptr CDhallTypeSpec
funcResultSpec p = p `plusPtr` #{offset cdhall_func_spec, resultSpec}

data CDhallFieldSpec = CDhallFieldSpec {
    fieldName :: CString,
    fieldOffset :: CSize,
    fieldType :: CDhallTypeSpec
  }

instance Storable CDhallFieldSpec
  where
    peek p =
      do
        fieldName <- #{peek cdhall_field_spec, name} p
        fieldOffset <- #{peek cdhall_field_spec, offset} p
        fieldType <- #{peek cdhall_field_spec, type} p
        return CDhallFieldSpec {..}
    poke = undefined
    sizeOf _ = #{size cdhall_field_spec}
    alignment _ = #{alignment cdhall_field_spec}

data CDhallUItemSpec = CDhallUItemSpec {
    uitemName :: CString,
    uitemType :: CDhallTypeSpec
  }

instance Storable CDhallUItemSpec
  where
    peek p =
      do
        uitemName <- #{peek cdhall_uitem_spec, name} p
        uitemType <- #{peek cdhall_uitem_spec, type} p
        return CDhallUItemSpec {..}
    poke = undefined
    sizeOf _ = #{size cdhall_uitem_spec}
    alignment _ = #{alignment cdhall_uitem_spec}


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
    tFunction = CDHALL_TYPE_FUNCTION
}

#{enum CDhallInt,,
    coptNone = CDHALL_OPTIONAL_NONE,
    coptSome = CDHALL_OPTIONAL_SOME}


--
-- Error codes
--
type ErrorCode = #{type cdhall_error_code}

#{enum ErrorCode,,
    eCDHALL_ERROR_EMPTY = CDHALL_ERROR_EMPTY,
    eCDHALL_ERROR_INVALID_TYPE = CDHALL_ERROR_INVALID_TYPE,
    eCDHALL_ERROR_ARITH_OVERFLOW = CDHALL_ERROR_ARITH_OVERFLOW,
    eCDHALL_ERROR_ARITH_UNDERFLOW = CDHALL_ERROR_ARITH_UNDERFLOW,
    eCDHALL_ERROR_ARITH_LOSS_OF_PRECISION = CDHALL_ERROR_ARITH_LOSS_OF_PRECISION,
    eCDHALL_ERROR_ARITH_DIVIDE_BY_ZERO = CDHALL_ERROR_ARITH_DIVIDE_BY_ZERO,
    eCDHALL_ERROR_ARITH_DENORMAL = CDHALL_ERROR_ARITH_DENORMAL,
    eCDHALL_ERROR_ARITH_RATIO_ZERO_DENOMINATOR = CDHALL_ERROR_ARITH_RATIO_ZERO_DENOMINATOR
}

