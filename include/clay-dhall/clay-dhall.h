#pragma once

/*!
\mainpage

\section MainpageDescription About clay-dhall
Dhall(https://github.com/dhall-lang/dhall-lang), developed by Gabriel Gonzalez, is strictly typed,
total (guaranteed to terminate, not turing complete) computer language.
Dhall is a good language to describe data, configuration, and so on.

This library is a C binding of Dhall, which calls the Haskell implementation of Dhall via FFI.
Dhall values are mapped directly into C memory representation described by the *spec* structure.

\section MainpageQuickStart Quick Start
(Under construction)

\section MainpageFirstExample First Example
\copydoc FirstExample

\section MainpageConcepts Concepts
\subsection MainpaceConceptsobjptr objptr
`cdhall_objptr` is some allocated object. It needs cleanup with `cdhall_free_objptr`.
`cdhall_clone_objptr` can duplicate an object. Actually this doesn't full copy but increment the reference counter. Note that all of the object that pointed by `cdhall_clone_objptr` is immutable.
*/

#ifdef __cplusplus
extern "C" {
#endif

#include "stdbool.h"
#include "type.h"

#ifdef __MINGW32__
#  define STDCALL __stdcall
#  ifdef CDHALL_DLL_IMPL
#    define DLL_EXPORT __declspec(dllexport)
#  else
#    define DLL_EXPORT __declspec(dllimport)
#  endif
#else
#  define DLL_EXPORT
#  define STDCALL
#endif


/*! -------------------
    \defgroup group_general Library initialization, termination, and resource managements.
    \details
    Call \ref cdhall_init before all the other library function's call.

    And call \ref cdhall_exit after all of them.
 */
/* \{ */

//! Initialize the library
DLL_EXPORT void STDCALL cdhall_init(void);

//! Initialize the library with GHC RTS arguments.
//
// \note Mainly for debugging. Acceptable options are depends on GHC  version. \n
// See the Grasgow Haskell Compiler manual for details.
DLL_EXPORT void STDCALL cdhall_init_with_args(int argc, char* argv[]);

//! Terminate the library
DLL_EXPORT void STDCALL cdhall_exit(void);

//! Perform garbage collection
DLL_EXPORT void STDCALL cdhall_perform_gc(void);

//! A synonym of malloc.
DLL_EXPORT void* STDCALL cdhall_alloc_array(size_t size);

//! A synonym of free.\n
// Used to finalize memory blocks(array, string) returned by the library.
DLL_EXPORT void STDCALL cdhall_free_array(void* ptr);

DLL_EXPORT cdhall_objptr STDCALL cdhall_clone_object(cdhall_objptr ptr);

DLL_EXPORT void STDCALL cdhall_free_object(cdhall_objptr ptr);

DLL_EXPORT bool STDCALL cdhall_call_func(const cdhall_objptr ptr, const void* arg, void* dest);

/* \} */

/*! -------------------
    \defgroup group_error Getting error message
 */
/* \{ */

DLL_EXPORT cdhall_error_code STDCALL cdhall_last_error_code();

DLL_EXPORT const char* STDCALL cdhall_last_error_message();

/* \} */

/*! -------------------
    \defgroup group_prettyprinting Pretty Printing
 */
/* \{ */

DLL_EXPORT char* STDCALL cdhall_show_expr_simple(cdhall_objptr expr);

/* \} */

/*! -------------------
    \defgroup group_exec Dhall execution
 */
/* \{ */

DLL_EXPORT bool STDCALL cdhall_input(const char* str, cdhall_typed_ptr holder);

DLL_EXPORT bool STDCALL cdhall_input_with_settings(cdhall_objptr stg, const char* str, cdhall_typed_ptr holder);

DLL_EXPORT bool STDCALL cdhall_input_file(const char* fileName, cdhall_typed_ptr holder);

DLL_EXPORT bool STDCALL cdhall_input_file_with_settings(cdhall_objptr stg, const char* fileName, cdhall_typed_ptr holder);

DLL_EXPORT cdhall_objptr STDCALL cdhall_input_expr(const char* str);

// DLL_EXPORT cdhall_objptr cdhall_input_expr_with_setting(cdhall_objptr stg, const char* str);

DLL_EXPORT bool STDCALL cdhall_extract(cdhall_objptr ptr, cdhall_typed_ptr holder);

DLL_EXPORT cdhall_objptr STDCALL cdhall_embed(cdhall_typed_ptr holder);

/* \} */

/*! -------------------
  \defgroup group_manip A few APIs to manipulate dhall values
*/
/* \{ */

DLL_EXPORT bool STDCALL cdhall_expr_eq(cdhall_objptr x, cdhall_objptr y);

#if 0
DLL_EXPORT cdhall_hashval cdhall_expr_hash(cdhall_objptr x);
#endif

/* \} */

/*! -------------------
    \defgroup group_customize Customize execution environment
 */
/* \{ */

DLL_EXPORT cdhall_objptr STDCALL cdhall_new_input_settings();

DLL_EXPORT cdhall_objptr STDCALL cdhall_new_evaluate_settings();

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
