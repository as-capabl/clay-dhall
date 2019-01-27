#include <assert.h>
#include <string.h>
#include <stddef.h>
#include <stdlib.h>
#include <clay-dhall/clay-dhall.h>
#include <clay-dhall/macro.h>

int main()
{
    cdhall_init(NULL, NULL);
    
    // String ----
    char* str = NULL;
    assert(!cdhall_input("+1", CDHALL_TPTR_SIMPLE(&str)));
    assert(cdhall_last_error_code() == CDHALL_ERROR_INVALID_TYPE);
    assert(cdhall_last_error_message() != NULL);

    // Double ----
    cdhall_uint n;
    assert(!cdhall_input("-1", CDHALL_TPTR_SIMPLE(&n)));
    assert(cdhall_last_error_code() == CDHALL_ERROR_ARITH_UNDERFLOW);
    assert(cdhall_last_error_message() != NULL);

    // Call `cdhall_exit` after all the other API call.
    cdhall_exit();

    return 0;
}
