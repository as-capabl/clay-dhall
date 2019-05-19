#pragma once

#ifdef __cplusplus
#  error "This header file doesn't support C++."
#endif

#define CDHALL_TYPEID_SIMPLE(t) \
    _Generic(t, \
        bool : CDHALL_TYPE_BOOL, \
        cdhall_uint : CDHALL_TYPE_NAT, \
        cdhall_int : CDHALL_TYPE_INT, \
        char* : CDHALL_TYPE_STRING, \
        double : CDHALL_TYPE_DOUBLE)

#define CDHALL_TPTR_SIMPLE(ptr) (cdhall_typed_ptr){{CDHALL_TYPEID_SIMPLE(*ptr), NULL}, ptr}

#define CDHALL_DEF_RECORD(strct,flds) {sizeof(flds)/sizeof(flds[0]), sizeof(*(strct*)NULL), flds}
#define CDHALL_DEF_FIELD(strct,typeid,tdata,member) {#member, offsetof(strct, member), {typeid, tdata}}
#define CDHALL_DEF_FIELD_SIMPLE(strct,member) {#member, offsetof(strct, member), {CDHALL_TYPEID_SIMPLE(((strct*)NULL)->member), NULL}}

#define CDHALL_DEF_UNION(uni,flds) {sizeof(flds)/sizeof(flds[0]), sizeof(*(uni*)NULL), flds}
#define CDHALL_DEF_UITEM(uni,typeid,tdata,member) {#member, {typeid, tdata}}
#define CDHALL_DEF_UITEM_SIMPLE(uni,member) {#member, {CDHALL_TYPEID_SIMPLE(((uni*)NULL)->member), NULL}}
