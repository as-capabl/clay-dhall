#include <assert.h>
#include <string.h>
#include <stddef.h>
#include <stdlib.h>
#include <clay-dhall/clay-dhall.h>
#include <clay-dhall/macro.h>

int main()
{
    cdhall_init(NULL, NULL);
    
    // Bool
    bool b;
    cdhall_typed_ptr hB = {{CDHALL_TYPE_BOOL, NULL}, &b};
    cdhall_input("True || False", hB);
    assert(b);
    
    bool b2;
    cdhall_input("True && False", CDHALL_TPTR_SIMPLE(&b2));
    assert(!b2);

    // Natural
    cdhall_uint n;
    cdhall_input("2*5", CDHALL_TPTR_SIMPLE(&n));
    assert(n == 10);

    // Integer
    cdhall_int i;
    cdhall_input("+42", CDHALL_TPTR_SIMPLE(&i));
    assert(i == 42);

    // String
    char* str = NULL;
    cdhall_input("\"This is \" ++ \"string\"", CDHALL_TPTR_SIMPLE(&str));
    assert(strcmp(str, "This is string") == 0);
    cdhall_free_array(str);

    // Double
    double d;
    cdhall_input("9.0", CDHALL_TPTR_SIMPLE(&d));
    assert(d == 9.0);

    // Unit
    cdhall_typed_ptr hUnit = {{CDHALL_TYPE_UNIT, NULL}, NULL};
    assert(cdhall_input("{=}", hUnit));

    cdhall_exit();

    return 0;
}
