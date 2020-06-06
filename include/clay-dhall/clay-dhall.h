//! \file clay-dhall.h

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
`cdhall_objptr` and derived types point some allocated objects.
They need cleanup with `cdhall_free_objptr`.
`cdhall_clone_objptr` can duplicate an object.
Actually this doesn't full copy but increment the reference counter.

Note that all of the object that pointed by `cdhall_clone_objptr` is immutable.
So modification operations take cdhall_clone_objptr* argument and will modify pointer address.
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

//! Clone a pointer to object.
DLL_EXPORT cdhall_objptr STDCALL cdhall_clone_object(cdhall_objptr ptr);

//! Free a pointer to object
DLL_EXPORT void STDCALL cdhall_free_object(cdhall_objptr ptr);

//! Call function
DLL_EXPORT bool STDCALL cdhall_call_func(const cdhall_funptr ptr, const void* arg, void* dest);

/* \} */

/*! -------------------
    \defgroup group_error Getting error message
 */
/* \{ */

//! Get error code of the last failured function call
DLL_EXPORT cdhall_error_code STDCALL cdhall_last_error_code(void);

//! Get error string of the last failured function call
// \note The pointer to storing is valid until the next call of any clay-dhall API.
DLL_EXPORT const char* STDCALL cdhall_last_error_message(void);

/* \} */

/*! -------------------
    \defgroup group_prettyprinting Pretty Printing
 */
/* \{ */

//! Prettyprint an expression to simple form.
DLL_EXPORT char* STDCALL cdhall_show_expr_simple(cdhall_expr expr);

/* \} */

/*! -------------------
    \defgroup group_exec Dhall execution
 */
/* \{ */

//! Input value from dhall source string.
DLL_EXPORT bool STDCALL cdhall_input(const char* str, cdhall_typed_ptr var);

//! Input value from dhall source string.
DLL_EXPORT bool STDCALL cdhall_input_with_settings(cdhall_input_settings stg, const char* str, cdhall_typed_ptr holder);

//! Input value from dhall source file.
DLL_EXPORT bool STDCALL cdhall_input_file(const char* fileName, cdhall_typed_ptr var);

//! Input value from dhall source file.
DLL_EXPORT bool STDCALL cdhall_input_file_with_settings(cdhall_evaluate_settings stg, const char* fileName, cdhall_typed_ptr holder);

//! Input an expression from dhall source string without evaluation.
DLL_EXPORT cdhall_expr STDCALL cdhall_input_expr(const char* str);

// DLL_EXPORT cdhall_objptr cdhall_input_expr_with_setting(cdhall_objptr stg, const char* str);

//! Evaluate an expression to its value.
DLL_EXPORT bool STDCALL cdhall_extract(cdhall_expr ptr, cdhall_typed_ptr holder);

//! Make an expression from its value.
DLL_EXPORT cdhall_expr STDCALL cdhall_embed(cdhall_typed_ptr holder);

/* \} */

/*! -------------------
  \defgroup group_manip A few APIs to manipulate dhall values
*/
/* \{ */

DLL_EXPORT bool STDCALL cdhall_expr_eq(cdhall_expr x, cdhall_expr y);

/* \} */

/*! -------------------
    \defgroup group_customize Customize execution environment
 */
/* \{ */

//! Create an input setting object that points the default setting.
DLL_EXPORT cdhall_objptr STDCALL cdhall_new_input_settings();

//! Create an evaluate setting object that points the default setting.
DLL_EXPORT cdhall_objptr STDCALL cdhall_new_evaluate_settings();
//! Create an input setting object that points the default setting.
DLL_EXPORT cdhall_input_settings STDCALL cdhall_new_input_settings();

//! Create an evaluate setting object that points the default setting.
DLL_EXPORT cdhall_evaluate_settings STDCALL cdhall_new_evaluate_settings();

//! Set root directory of the evaluate setting object.
DLL_EXPORT void STDCALL cdhall_set_root_directory(cdhall_input_settings* stg, const char* dir);

//! Type to point custom builtin function
typedef cdhall_objptr (*cdhall_builtin_func)(void*, const cdhall_objptr*);

//! Add builtin function.
DLL_EXPORT bool cdhall_add_builtin(
    cdhall_has_evaluate_settings* stg,
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
