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

    // Optional
    cdhall_union* pOpt = malloc(CDHALL_UNION_REQUIRED_SIZE(cdhall_uint));
    pOpt->index = CDHALL_OPTIONAL_SOME;
    *((cdhall_int*)pOpt->data) = 123;
    cdhall_typed_ptr hOpt = {{CDHALL_TYPE_OPTIONAL, &hArrayElem}, pOpt};
    cdhall_objptr exprSome1 = cdhall_embed(hOpt);
    cdhall_objptr exprSome2 = cdhall_input_expr("Some 1");
    pOpt->index = CDHALL_OPTIONAL_NONE;
    cdhall_objptr exprNone1 = cdhall_embed(hOpt);
    cdhall_objptr exprNone2 = cdhall_input_expr("None : Optional Natural");
    assert(cdhall_expr_eq(exprSome1, exprSome2));
    assert(cdhall_expr_eq(exprNone1, exprNone2));
    assert(cdhall_expr_eq(exprSome1, exprNone1));
    cdhall_free_object(exprSome1);
    cdhall_free_object(exprSome2);
    cdhall_free_object(exprNone1);
    cdhall_free_object(exprNone2);
    free(pOpt);

    // Record
    test_struct ts1 = { 1, 10.0 };
    cdhall_typed_ptr hTs1 = { {CDHALL_TYPE_RECORD, &test_struct_rec}, &ts1 };
    cdhall_objptr exprTs1 = cdhall_embed(hTs1);
    cdhall_objptr exprTs2 = cdhall_input_expr("{ testI = +1, testD = 10.0 }");
    assert(cdhall_expr_eq(exprTs1, exprTs2));
    cdhall_free_object(exprTs1);
    cdhall_free_object(exprTs2);

    // Union
    cdhall_union* pU = malloc(CDHALL_UNION_REQUIRED_SIZE(test_union));
    pU->index = 1;
    ((test_union*)pU->data)->ts = (test_struct){ 2, 11.0 };
    cdhall_typed_ptr hU = {{CDHALL_TYPE_UNION, &test_union_spec}, pU};
    cdhall_objptr exprU1 = cdhall_embed(hU);
    cdhall_objptr exprU2 = cdhall_input_expr("<Ts = {testI = +2, testD = 11.0} | I : Integer>");
    assert(cdhall_expr_eq(exprU1, exprU2));
    cdhall_free_object(exprU1);
    cdhall_free_object(exprU2);
    free(pU);

    cdhall_exit();
}