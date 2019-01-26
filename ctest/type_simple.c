#include <assert.h>
#include <string.h>
#include <stddef.h>
#include <stdlib.h>
#include <clay-dhall/clay-dhall.h>
#include <clay-dhall/macro.h>

int main()
{
    // Call `cdhall_init` until all the other API call.
    cdhall_init(NULL, NULL);
    
    // **** How to input a value by interpreting a Dhall expression *****
    //   Let's interpret an expression;
    //      True || False,
    //   whose expected type is `Bool`

    //   Step 1. Construct `cdhall_type_spec` to describe a type you expect.
    //     The first member `typeId` specifies the type. Second member `detail` 
    //     is used to describe compound types; in the case of `Bool`, set `NULL`.
    //     
    //     Full description to encode Dhall types to type specs is available
    //     at the documentation of `cdhall_type_spec`
    cdhall_type_spec specBool = {CDHALL_TYPE_BOOL, NULL};

    //   Step 2. Prepare a memoly place to store an input value of the type.
    //     The correspondence of C types and Dhall types is also available in
    //     the documentation of `cdhall_type_spec`.
    //
    //     For `Bool`, define a variable of type `bool`.
    bool b;

    //   Step 3. Construct `cdhall_typed_ptr`, a typed pointer,
    //     by putting together with the pointer to the store place and the type spec.
    cdhall_typed_ptr hB = {specBool, &b};

    //   Step 4. Pass the typed pointer to some input API 
    cdhall_input("True || False", hB);

    //   Then, the result is stored at the memory place.
    assert(b); // b == (True || False) == True
    
    // For some simple types, you can make typed pointer immediately, using generic macro
    // defined at `macro.h`.
    //
    // The rest part is the list of them.

    // Bool ----
    bool b2;
    cdhall_input("True && False", CDHALL_TPTR_SIMPLE(&b2));
    assert(!b2);

    // Natural ----
    cdhall_uint n;
    cdhall_input("2*5", CDHALL_TPTR_SIMPLE(&n));
    assert(n == 10);

    // Integer ----
    cdhall_int i;
    cdhall_input("+42", CDHALL_TPTR_SIMPLE(&i));
    assert(i == 42);

    // String ----
    char* str = NULL;
    cdhall_input("\"This is \" ++ \"string\"", CDHALL_TPTR_SIMPLE(&str));
    assert(strcmp(str, "This is string") == 0);
    cdhall_free_array(str); // Free the memory.

    // Double ----
    double d;
    cdhall_input("9.0", CDHALL_TPTR_SIMPLE(&d));
    assert(d == 9.0);

    // Unit ----
    cdhall_typed_ptr hUnit = {{CDHALL_TYPE_UNIT, NULL}, NULL};
    assert(cdhall_input("{=}", hUnit));

    // ----

    // Call `cdhall_exit` after all the other API call.
    cdhall_exit();

    return 0;
}
