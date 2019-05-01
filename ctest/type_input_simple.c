#include <assert.h>
#include <string.h>
#include <stddef.h>
#include <stdlib.h>
#include <clay-dhall/clay-dhall.h>
#include <clay-dhall/macro.h>

int main()
{
    cdhall_init(NULL, NULL);

    cdhall_uint n = 30;
    cdhall_objptr exprN1 = cdhall_embed(CDHALL_TPTR_SIMPLE(&n));
    cdhall_objptr exprN2 = cdhall_input_expr("30");
    cdhall_objptr exprN3 = cdhall_input_expr("40");
    assert(cdhall_expr_eq(exprN1, exprN2));
    assert(!cdhall_expr_eq(exprN1, exprN3));
    cdhall_free_object(exprN1);
    cdhall_free_object(exprN2);
    cdhall_free_object(exprN3);

    cdhall_exit();
}