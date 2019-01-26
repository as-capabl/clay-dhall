#include <assert.h>
#include <string.h>
#include <stddef.h>
#include <clay-dhall/clay-dhall.h>
#include <clay-dhall/macro.h>


static const cdhall_type_spec mult_double_args[] = {
    {CDHALL_TYPE_DOUBLE, NULL},
    {CDHALL_TYPE_DOUBLE, NULL},
};

static const cdhall_type_spec mult_double_ret = {CDHALL_TYPE_DOUBLE, NULL};

cdhall_objptr mult_double_impl(void* p, const cdhall_objptr* pArg)
{
    double x, y;
    cdhall_extract(pArg[0], CDHALL_TPTR_SIMPLE(&x));
    cdhall_extract(pArg[1], CDHALL_TPTR_SIMPLE(&y));

    double r = x * y;
    return cdhall_embed(CDHALL_TPTR_SIMPLE(&r));
}


int main()
{
    cdhall_init(NULL, NULL);

    cdhall_objptr stg = cdhall_new_input_settings();
    cdhall_add_builtin(&stg, "mult_double", 2, mult_double_args, mult_double_ret, NULL, NULL, mult_double_impl);
    
    double d;
    cdhall_input_with_settings(stg, "mult_double 2.0 0.1", CDHALL_TPTR_SIMPLE(&d));
    assert(d == 0.2);

    cdhall_free_object(stg);

    
    cdhall_exit();
    return 0;
}