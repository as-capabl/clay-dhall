#include "assert.h"
#include "string.h"
#include "stddef.h"
#include <stdlib.h>
#include <clay-dhall/clay-dhall.h>
#include <clay-dhall/macro.h>

int main()
{
    cdhall_init();
    
    // `cdhall_input_expr` returns expression without evaluation.
    // Then `cdhall_extract` evaluates the expression and handle the result.
    cdhall_objptr exprN = cdhall_input_expr("3*5");
    assert(exprN);

    cdhall_uint n;
    cdhall_extract(exprN, CDHALL_TPTR_SIMPLE(&n));
    assert(n == 15);

    // `cdhall_clone_object` duplicates a reference
    cdhall_objptr exprN_clone = cdhall_clone_object(exprN);
    cdhall_free_object(exprN);
    n = 0;
    cdhall_extract(exprN_clone, CDHALL_TPTR_SIMPLE(&n));
    assert(n == 15);
    cdhall_free_object(exprN_clone);

    // `cdhall_free_object(NULL)` is safe.
    cdhall_free_object(NULL);

    // `cdhall_embed` makes an expression from a value.
    cdhall_uint n2In = 81;
    cdhall_objptr exprN2 = cdhall_embed(CDHALL_TPTR_SIMPLE(&n2In));
    // assert(exprN2);

    cdhall_uint n2Out = 0;
    cdhall_extract(exprN2, CDHALL_TPTR_SIMPLE(&n2Out));
    assert(n2In == n2Out);
    cdhall_free_object(exprN2);

    cdhall_exit();

    return 0;
}
