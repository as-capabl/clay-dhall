#pragma once
#include <stdint.h>


typedef int_fast32_t cdhall_int;
typedef uint_fast32_t cdhall_uint;

/*! -------------------
    \defgroup group_type_spec Type spec descriptors.

    \details
    Describe the mapping between types of Dhall and host language.

    Value of `typeId`   | Value of `detail`| C Type       | Finalizer for C Type
    --------------------|------------------|--------------|----------------------
    CDHALL_TYPE_BOOL    | (not concerned)  | bool         | -
    CDHALL_TYPE_NAT     | (not concerned)  | cdhall_uint  | -
    CDHALL_TYPE_INT     | (not concerned)  | cdhall_int   | -
    CDHALL_TYPE_STRING  | (not concerned)  | char*        | cdhall_free_array
    CDHALL_TYPE_DOUBLE  | (not concerned)  | double       | -
    CDHALL_TYPE_ARRAY   | cdhall_type_spec*| cdhall_array | cdhall_free_array
    CDHALL_TYPE_UNIT    | (not concerned)  | (none)       | -
    CDHALL_TYPE_OPTION  |||
    CDHALL_TYPE_RECORD  | cdhall_record_spec*| T          | -
    CDHALL_TYPE_UNION   | cdhall_union_spec*| cdhall_union | -
 */
/* \{ */
typedef enum {
    CDHALL_TYPE_BOOL,
    CDHALL_TYPE_NAT,
    CDHALL_TYPE_INT,
    CDHALL_TYPE_STRING,
    CDHALL_TYPE_DOUBLE,
    CDHALL_TYPE_ARRAY,
    CDHALL_TYPE_UNIT,
    CDHALL_TYPE_OPTIONAL,
    CDHALL_TYPE_RECORD,
    CDHALL_TYPE_UNION,
    CDHALL_TYPE_FUNAPP
} cdhall_type;

typedef struct {
    cdhall_type  typeId;
    const void  *detail;
} cdhall_type_spec;

typedef struct {
    const char*  name;
    size_t  offset;
    cdhall_type_spec  type;
} cdhall_field_spec;

typedef struct {
    cdhall_int  numFields;
    size_t  byteSize;
    const  cdhall_field_spec  *pFields;
} cdhall_record_spec;

typedef struct {
    const char*  name;
    cdhall_type_spec  type; 
} cdhall_uitem_spec;

typedef struct {
    cdhall_int  numItems;
    size_t  byteSize;
    const cdhall_uitem_spec  *pItems;
} cdhall_union_spec;

typedef struct {
    cdhall_type_spec  argSpec;
    cdhall_type_spec  resultSpec;
} cdhall_funapp;

/*! \} */

/*! -------------------
    \defgroup group_host_type Host types.
    
    \details
    Host types mapped to Dhall values. See \ref group_type_spec about the mapping.
 */
/* \{ */

typedef struct {
    cdhall_type_spec  spec;
    void  *ptr;
} cdhall_typed_ptr;

typedef struct {
    size_t size;
    void* elem;
} cdhall_array;

typedef struct {
    cdhall_int  index;
    char  data[1];
} cdhall_union;

#define CDHALL_UNION_REQUIRED_SIZE(t)  (offsetof(cdhall_union, data) + sizeof(t))

typedef enum {
    CDHALL_OPTIONAL_NONE = 0,
    CDHALL_OPTIONAL_SOME
} cdhall_optional_index;

/*! \} */

/*! -------------------
    \defgroup group_error_notif Error Notifications.

    \note This library do not handle unknown exception.

    APIs throw exceptions in some cases like below.

    - Exception thrown by builtin function
    - Exception via -fnon-call-exceptions
 */
/* \{ */
typedef enum {
    CDHALL_ERROR_EMPTY,

    // Dhall.InvalidType
    CDHALL_ERROR_INVALID_TYPE,

    // ArithException
    CDHALL_ERROR_ARITH_OVERFLOW, // Overflow	 
    CDHALL_ERROR_ARITH_UNDERFLOW, // Underflow	 
    CDHALL_ERROR_ARITH_LOSS_OF_PRECISION, // LossOfPrecision	 
    CDHALL_ERROR_ARITH_DIVIDE_BY_ZERO, // DivideByZero	 
    CDHALL_ERROR_ARITH_DENORMAL, // Denormal	 
    CDHALL_ERROR_ARITH_RATIO_ZERO_DENOMINATOR, // RatioZeroDenominator
} cdhall_error_code;
/*! \} */

//
// Managed objects
//
typedef struct cdhall_obj_ {} *cdhall_objptr; // struct is dummy; To avoid implicit conversion.
