First Example {#FirstExample}
----------------------------------------------------------

\dontinclude type_simple.c

The tutorial of Dhall language is available here.
https://hackage.haskell.org/package/dhall/docs/Dhall-Tutorial.html

This documentation is about C bindings of it.

As the first example, let's interpret an expression `True || False`
whose expected type is `Bool`


clay-dhall's include file is below.

\skipline clay-dhall.h
\line     macro.h

Call `cdhall_init` before all the other API call.

\skipline cdhall_init


Then let's start interpretation.
In this example the resulting value is `Bool`.

You should consult the table at the \ref group_type_spec page.

For the type `Bool`, you can get information below.

Information       | Value
-----------       | --------
Dhall Type        | Bool
Value of `typeId` | CDHALL_TYPE_BOOL
Value of `detail` | (Not concerned)
C Type            | bool
Finalizer for C Type | -

Before interpreting an expression, several things need to be prepared along with this table.


First, construct `cdhall_type_spec` to describe a type you expect.
The first member `typeId` specifies the type. Second member `detail` 
is used to describe compound types.

Appropreate value for `Bool` is already shown at the table above.

\skipline cdhall_type_spec

Note that `detail` is not concerned so it may be `NULL`.


Next, prepare a memory place to store an input value of the type.

For `Bool`, the corresponding C type is `bool`.

\skipline bool b


Then you can construct `cdhall_typed_ptr`, a typed pointer
by putting together with the pointer to the store place and the type spec.

\skipline cdhall_typed_ptr

Now you can receive the interpreted value by passing the typed pointer to the input API.

\skipline cdhall_input
\skipline assert(b)


Actually, for simple type, typed pointer can be constructed immediately using a generic macro.
'Simple type' means whose value of `detail` is not concerned.

\skip bool b2
\until assert