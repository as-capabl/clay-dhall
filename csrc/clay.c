#include <stdio.h>
#include <stdlib.h>

#define CDHALL_DLL_IMPL
#include <clay-dhall/clay-dhall.h>
#include "Clay/Export_stub.h"


DLL_EXPORT void cdhall_init(int* argc, char** argv[])
{
    hs_init(argc, argv);
}

DLL_EXPORT void cdhall_exit()
{
    hs_exit();
}

DLL_EXPORT void* cdhall_alloc_array(size_t size)
{
    return malloc(size);
}

DLL_EXPORT void cdhall_free_array(void* ptr)
{
    free(ptr);
}

DLL_EXPORT cdhall_objptr cdhall_clone_object(cdhall_objptr ptr)
{
    return hsc_clone_object(ptr);
}

DLL_EXPORT void cdhall_free_object(cdhall_objptr ptr)
{
    hs_free_stable_ptr((HsStablePtr)ptr);
}

DLL_EXPORT bool cdhall_call_func(const cdhall_objptr ptr, const void* arg, void* dest)
{
    return hsc_call_func((HsStablePtr)ptr, (void*)arg, dest) != HS_BOOL_FALSE;
}

DLL_EXPORT bool cdhall_input(const char* str, cdhall_typed_ptr holder)
{
    return hsc_input((HsPtr)str, (HsPtr)&holder) != HS_BOOL_FALSE;
}

DLL_EXPORT bool cdhall_input_with_settings(cdhall_objptr stg, const char* str, cdhall_typed_ptr holder)
{
    return hsc_input_with_settings((HsStablePtr)stg, (HsPtr)str, (HsPtr)&holder) != HS_BOOL_FALSE;
}


DLL_EXPORT cdhall_objptr cdhall_input_expr(const char* str)
{
    return (cdhall_objptr)hsc_input_expr((HsPtr)str);
}

DLL_EXPORT bool cdhall_extract(cdhall_objptr expr, cdhall_typed_ptr holder)
{
    return hsc_extract((HsStablePtr)expr, (HsPtr)&holder) != HS_BOOL_FALSE;
}

DLL_EXPORT cdhall_objptr cdhall_embed(cdhall_typed_ptr holder)
{
    return hsc_embed((HsPtr)&holder);
}

DLL_EXPORT cdhall_objptr cdhall_new_evaluate_settings()
{
    return hsc_new_evaluate_settings();
}

DLL_EXPORT cdhall_objptr cdhall_new_input_settings()
{
    return hsc_new_input_settings();
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
DLL_EXPORT bool cdhall_add_builtin(
    cdhall_objptr* stg,
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

