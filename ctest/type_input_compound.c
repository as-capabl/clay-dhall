#include <assert.h>
#include <string.h>
#include <stddef.h>
#include <stdlib.h>
#include <clay-dhall/clay-dhall.h>
#include <clay-dhall/macro.h>

// Test struct
typedef struct {
    cdhall_int testI;
    double testD;
} test_struct;

static const cdhall_field_spec test_struct_flds[] = {
    CDHALL_DEF_FIELD_SIMPLE(test_struct, testI),
    CDHALL_DEF_FIELD_SIMPLE(test_struct, testD),
};
static const cdhall_record_spec test_struct_rec =
    CDHALL_DEF_RECORD(test_struct, test_struct_flds);

// Test union
typedef union {
    cdhall_int  i;
    test_struct  ts;
} test_union;

static const cdhall_uitem_spec test_union_items[] = {
    {"I", {CDHALL_TYPE_INT, NULL}},
    {"Ts", {CDHALL_TYPE_RECORD, &test_struct_rec}}
};

static const cdhall_union_spec test_union_spec = {2, sizeof(test_union), test_union_items};


int main()
{
    cdhall_init(NULL, NULL);

    // Array
    cdhall_uint arrayElem[] = {1, 2, 3};
    cdhall_array array = { sizeof(arrayElem)/sizeof(arrayElem[0]), arrayElem };
    cdhall_type_spec hArrayElem = {CDHALL_TYPE_NAT, NULL};
    cdhall_typed_ptr hArray = {{CDHALL_TYPE_ARRAY, &hArrayElem}, &array};
    cdhall_objptr exprArray = cdhall_embed(hArray);
    cdhall_objptr exprArray2 = cdhall_input_expr("[1,2,3]");
    assert(cdhall_expr_eq(exprArray, exprArray2));
    cdhall_free_object(exprArray);
    cdhall_free_object(exprArray2);

    // Record
    test_struct ts1 = { 1, 10.0 };
    cdhall_typed_ptr hTs1 = { {CDHALL_TYPE_RECORD, &test_struct_rec}, &ts1 };
    cdhall_objptr exprTs1 = cdhall_embed(hTs1);
    cdhall_objptr exprTs2 = cdhall_input_expr("{ testI = +1, testD = 10.0 }");
    assert(cdhall_expr_eq(exprTs1, exprTs2));
    cdhall_free_object(exprTs1);
    cdhall_free_object(exprTs2);

    cdhall_exit();
}