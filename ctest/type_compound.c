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
    cdhall_init();

    // Array
    cdhall_array array;
    cdhall_type_spec hArrayElem = {CDHALL_TYPE_NAT, NULL};
    cdhall_typed_ptr hArray = {{CDHALL_TYPE_ARRAY, &hArrayElem}, &array};
    cdhall_input("[1,2,3]", hArray);
    assert(array.size == 3);
    cdhall_uint* arrayData = (cdhall_uint*)array.elem;
    assert(arrayData[0] == 1);
    assert(arrayData[1] == 2);
    assert(arrayData[2] == 3);
    cdhall_free_array(arrayData);

    cdhall_array unitArray;
    cdhall_type_spec hUnitArrayElem = {CDHALL_TYPE_UNIT, NULL};
    cdhall_typed_ptr hUnitArray = {{CDHALL_TYPE_ARRAY, &hUnitArrayElem}, &unitArray};
    cdhall_input("[{=},{=},{=},{=},{=}]", hUnitArray);
    assert(unitArray.size == 5);
    cdhall_free_array(unitArray.elem);

    // Option
    cdhall_union* pOn = malloc(CDHALL_UNION_REQUIRED_SIZE(cdhall_uint)); 
    cdhall_typed_ptr hOn = {{CDHALL_TYPE_OPTIONAL, &hArrayElem}, pOn};
    cdhall_input("Some 123", hOn);
    assert(pOn->index == CDHALL_OPTIONAL_SOME);
    cdhall_uint nO = *(cdhall_uint*)pOn->data;
    assert(nO == 123);
    cdhall_input("None Natural", hOn);
    assert(pOn->index == CDHALL_OPTIONAL_NONE);
    free(pOn);

    // Record
    test_struct ts;
    cdhall_typed_ptr hTs = {{CDHALL_TYPE_RECORD, &test_struct_rec}, &ts};
    cdhall_input("{testI = +99, testD = 11.0}", hTs);
    assert(ts.testI == 99);
    assert(ts.testD == 11.0);

    // Union
    cdhall_union* pU = malloc(CDHALL_UNION_REQUIRED_SIZE(test_union));
    cdhall_typed_ptr hU = {{CDHALL_TYPE_UNION, &test_union_spec}, pU};
    cdhall_input("<Ts = {testI = +999, testD = 12.0} | I : Integer>", hU);
    assert(pU->index == 1);
    test_struct ts2 = ((test_union*)pU->data)->ts;
    assert(ts2.testI == 999);
    assert(ts2.testD == 12.0);
    free(pU);


    // Function
    cdhall_func_spec funapp = {
        {CDHALL_TYPE_NAT, NULL},
        {CDHALL_TYPE_NAT, NULL}
    };
    cdhall_objptr  fun1;
    cdhall_typed_ptr hFunapp = {{CDHALL_TYPE_FUNCTION, &funapp}, &fun1};
    cdhall_input("λ(n : Natural) -> n * 10", hFunapp);
    const cdhall_uint nIn = 12;
    cdhall_uint nOut;
    cdhall_call_func(fun1, &nIn, &nOut);
    cdhall_free_object(fun1);
    assert(nOut == 120);

    cdhall_type_spec dArraySpec = {CDHALL_TYPE_DOUBLE, NULL};
    cdhall_func_spec funapp2 = {
        {CDHALL_TYPE_RECORD, &test_struct_rec},
        {CDHALL_TYPE_ARRAY, &dArraySpec}
    };
    cdhall_objptr  fun2;
    cdhall_typed_ptr hFunapp2 = {{CDHALL_TYPE_FUNCTION, &funapp2}, &fun2};
    cdhall_input("λ(ts : {testI : Integer, testD : Double}) -> [ts.testD]", hFunapp2);
    const test_struct tsIn = {5, 20.0};
    cdhall_array arrayOut;
    cdhall_call_func(fun2, &tsIn, &arrayOut);
    cdhall_free_object(fun2);
    assert(arrayOut.size == 1);
    assert(((double*)arrayOut.elem)[0] == 20.0);
    cdhall_free_array(arrayOut.elem);

    cdhall_exit();

    return 0;
}
