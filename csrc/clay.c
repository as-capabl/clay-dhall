//! \file clay.c

#include <stdio.h>
#include <stdlib.h>

#define CDHALL_DLL_IMPL
#include <clay-dhall/clay-dhall.h>
#include "Clay/Export_stub.h"

cdhall_objptr  g_lasterror = NULL;

DLL_EXPORT void STDCALL cdhall_init(void)
{
    hs_init(NULL, NULL);
}

DLL_EXPORT void STDCALL cdhall_init_with_args(int argc, char* argv[])
{
    hs_init(&argc, &argv);
}

DLL_EXPORT void STDCALL cdhall_exit(void)
{
    if (g_lasterror) cdhall_free_object(g_lasterror);
    hs_exit();
}

DLL_EXPORT void STDCALL cdhall_perform_gc(void)
{
    hs_perform_gc();
}

DLL_EXPORT void* STDCALL cdhall_alloc_array(size_t size)
{
    return malloc(size);
}

DLL_EXPORT void STDCALL cdhall_free_array(void* ptr)
{
    free(ptr);
}

DLL_EXPORT cdhall_objptr STDCALL cdhall_clone_object(cdhall_objptr ptr)
{
    return hsc_clone_object(ptr);
}

DLL_EXPORT void STDCALL cdhall_free_object(cdhall_objptr ptr)
{
    hs_free_stable_ptr((HsStablePtr)ptr);
}

DLL_EXPORT bool STDCALL cdhall_call_func(const cdhall_objptr ptr, const void* arg, void* dest)
{
    return hsc_call_func((HsStablePtr)ptr, (void*)arg, dest) != HS_BOOL_FALSE;
}

DLL_EXPORT cdhall_error_code STDCALL cdhall_last_error_code(void)
{
    return hsc_last_error_code();
}

DLL_EXPORT const char* STDCALL cdhall_last_error_message(void)
{
    return hsc_last_error_message();
}

DLL_EXPORT char* STDCALL cdhall_show_expr_simple(cdhall_expr expr)
{
    return hsc_show_expr_simple((HsStablePtr)expr);
}


DLL_EXPORT bool STDCALL cdhall_input(const char* str, cdhall_typed_ptr var)
{
    return hsc_input((HsPtr)str, (HsPtr)&var) != HS_BOOL_FALSE;
}

DLL_EXPORT bool STDCALL cdhall_input_with_settings(cdhall_input_settings stg, const char* str, cdhall_typed_ptr var)
{
    return hsc_input_with_settings((HsStablePtr)stg, (HsPtr)str, (HsPtr)&var) != HS_BOOL_FALSE;
}

DLL_EXPORT bool STDCALL cdhall_input_file(const char* fileName, cdhall_typed_ptr var)
{
    return hsc_input_file((HsPtr)fileName, (HsPtr)&var) != HS_BOOL_FALSE;
}

DLL_EXPORT bool STDCALL cdhall_input_file_with_settings(cdhall_evaluate_settings stg, const char* fileName, cdhall_typed_ptr var)
{
    return hsc_input_file_with_settings((HsStablePtr)stg, (HsPtr)fileName, (HsPtr)&var) != HS_BOOL_FALSE;
}


DLL_EXPORT cdhall_expr STDCALL cdhall_input_expr(const char* str)
{
    return (cdhall_expr)hsc_input_expr((HsPtr)str);
}

DLL_EXPORT bool STDCALL cdhall_extract(cdhall_expr expr, cdhall_typed_ptr var)
{
    return hsc_extract((HsStablePtr)expr, (HsPtr)&var) != HS_BOOL_FALSE;
}

DLL_EXPORT cdhall_expr STDCALL cdhall_embed(cdhall_typed_ptr var)
{
    return hsc_embed((HsPtr)&var);
}

DLL_EXPORT bool STDCALL cdhall_expr_eq(cdhall_expr x, cdhall_expr y)
{
    return hsc_expr_eq((HsStablePtr)x, (HsStablePtr)y);
}

DLL_EXPORT cdhall_evaluate_settings STDCALL cdhall_new_evaluate_settings()
{
    return hsc_new_evaluate_settings();
}

DLL_EXPORT cdhall_input_settings STDCALL cdhall_new_input_settings()
{
    return hsc_new_input_settings();
}

DLL_EXPORT void STDCALL cdhall_set_root_directory(cdhall_input_settings* stg, const char* dir)
{
    hsc_set_root_directory((HsPtr)stg, (HsPtr) dir);
}

/*
type BuiltinImpl = Ptr UserData -> Ptr (StablePtr Obj) -> IO (StablePtr Obj)
foreign import ccall "dynamic"
  derefBuiltinImpl :: FunPtr BuiltinImpl -> BuiltinImpl
foreign export ccall hsc_add_builtin ::
    Ptr (StablePtr Obj) ->
    CString -> -- Builtin function name
    CDhallInt-> Ptr CDhallTypeHolder -> -- Arguments
    Ptr CDhallTypeHolder -> -- Result type
    FinalizerPtr UserData -> Ptr UserData -> FunPtr BuiltinImpl -> -- callback routine
    IO ()
*/
DLL_EXPORT bool STDCALL cdhall_add_builtin(
    cdhall_has_evaluate_settings* stg,
    const char* name,
    cdhall_int nArg,
    const cdhall_type_spec* argSpecs,
    cdhall_type_spec retSpec,
    void (*finalizer)(void*),
    void* userData,
    cdhall_builtin_func impl)
{
    hsc_add_builtin((HsPtr)stg, (HsPtr)name, nArg, (HsPtr)argSpecs, (HsPtr)&retSpec, (HsPtr)finalizer, userData, (HsPtr)impl);
    return true;
}

