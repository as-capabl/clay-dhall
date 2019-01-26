#pragma once

/*!
\mainpage

\section MainpageDescription Description
Dhall(https://github.com/dhall-lang/dhall-lang), developed by Gabriel Gonzalez, is strictly typed, deterministic (not turing complete) computer language.
Dhall is a good language to describe data, configuration, and so on.

This library is a C binding of Dhall, which calls the Haskell implementation of Dhall via FFI.
Dhall values are mapped directly into C memory representation described by the *spec* structure.


\section MainpageTutorial Tutorial
\dontinclude type_simple.c

See this example;
\skip main
\until return 0
\until }
*/

#ifdef __cplusplus
extern "C" {
#endif

#include "stdbool.h"
#include "type.h"

#ifdef __MINGW32__
#  ifdef CDHALL_DLL_IMPL
#    define DLL_EXPORT __declspec(dllexport)
#  else
#    define DLL_EXPORT __declspec(dllimport)
#  endif
#else
#  define DLL_EXPORT
#endif


/*! -------------------
    \defgroup group_general Library initialization, termination, and resource managements.
    \details
    Call \ref cdhall_init before all the other library function's call.

    And call \ref cdhall_exit after all of them.
 */
/* \{ */

//! Initialize the library
DLL_EXPORT void cdhall_init(int* argc, char** argv[]);

//! Terminate the library
DLL_EXPORT void cdhall_exit();

//! A synonym of malloc.
DLL_EXPORT void* cdhall_alloc_array(size_t size);

//! A synonym of free.\n
// Used to finalize memory blocks(array, string) returned by the library.
DLL_EXPORT void cdhall_free_array(void* ptr);

DLL_EXPORT cdhall_objptr cdhall_clone_object(cdhall_objptr ptr);

DLL_EXPORT void cdhall_free_object(cdhall_objptr ptr);

DLL_EXPORT bool cdhall_call_func(const cdhall_objptr ptr, const void* arg, void* dest);

/* \} */

/*! -------------------
    \defgroup group_error Getting error message
 */
/* \{ */



/* \} */


/*! -------------------
    \defgroup group_exec Dhall execution
 */
/* \{ */

DLL_EXPORT bool cdhall_input(const char* str, cdhall_typed_ptr holder);

DLL_EXPORT bool cdhall_input_with_settings(cdhall_objptr stg, const char* str, cdhall_typed_ptr holder);

// DLL_EXPORT bool cdhall_input_file(const char* str, cdhall_typed_ptr holder);

// DLL_EXPORT bool cdhall_input_file_with_settings(cdhall_objptr stg, const char* str, cdhall_typed_ptr holder);

DLL_EXPORT cdhall_objptr cdhall_input_expr(const char* str);

// DLL_EXPORT cdhall_objptr cdhall_input_expr_with_setting(cdhall_objptr stg, const char* str);

DLL_EXPORT bool cdhall_extract(cdhall_objptr ptr, cdhall_typed_ptr holder);

DLL_EXPORT cdhall_objptr cdhall_embed(cdhall_typed_ptr holder);

/* \} */

/*! -------------------
    \defgroup group_customize Customize execution environment
 */
/* \{ */

DLL_EXPORT cdhall_objptr cdhall_new_input_settings();

DLL_EXPORT cdhall_objptr cdhall_new_evaluate_settings();

typedef cdhall_objptr (*cdhall_builtin_func)(void*, const cdhall_objptr*);

DLL_EXPORT bool cdhall_add_builtin(
    cdhall_objptr* stg,
    const char* name,
    cdhall_int nArg,
    const cdhall_type_spec* argSpecs,
    cdhall_type_spec retSpec,
    void (*finalizer)(void*),
    void* userData,
    cdhall_builtin_func impl
);

/* \} */


#ifdef __cplusplus
}
#endif
