clay-dhall {#README}
=============================

C layer for Dhall language

Description
-----------------------------
Dhall(https://github.com/dhall-lang/dhall-lang), developed by Gabriel Gonzalez, is strictly typed, deterministic (not turing complete) computer language.
Dhall is a good language to describe data, configuration, and so on.

This library is a C binding of Dhall, which calls the Haskell implementation of Dhall via FFI.
Dhall values are mapped directly into C memory representation described by the *spec* structure.


Documentation
-----------------------------
The documentation is available at the URL below.

http://as-capabl.github.com/clay-dhall/


Current Status
-----------------------------
Working toward the 1st release.

- [ ] APIs to get error messages
- [ ] PerformGC API
- [ ] Full documentation
- [ ] Installation

-----

- [x] data EvaluateSettings
- [x] data InputSettings
- [x] data InputType a
  - [x] embed
  - [ ] declared
- [ ] data InterpretOptions
- [ ] data InvalidType
- [x] newtype RecordInputType a
- [x] newtype RecordType a
- [ ] data Type a
  - [x] extract
  - [ ] expected
- [x] auto :: Interpret a => Type a
- [x] bool :: Type Bool
- [x] defaultEvaluateSettings :: EvaluateSettings
- [x] defaultInputSettings :: InputSettings
- [ ] defaultInterpretOptions :: InterpretOptions
- [ ] detailed :: IO a -> IO a
- [x] double :: Type Double
- [x] field :: Text -> Type a -> RecordType a
- [x] genericAuto ::
- [ ] inject :: Inject a => InputType a
  - Contains `undefined`s
- [x] input :: Type a -> Text -> IO a
- [x] inputExpr ::
- [ ] inputExprWithSettings ::
- [x] inputField :: Inject a => Text -> RecordInputType a
- [x] inputFieldWith :: Text -> InputType a -> RecordInputType a
- [ ] inputFile :: Type a -> FilePath -> IO a
- [ ] inputFileWithSettings ::
- [x] inputRecord :: RecordInputType a -> InputType a
- [x] inputWithSettings :: InputSettings -> Type a -> Text -> IO a
- [x] integer :: Type Integer
- ns lazyText :: Type Data.Text.Internal.Lazy.Text
- ns list :: Type a -> Type [a]
- [x] Dhall.maybe :: Type a -> Type (Maybe a)
- [x] natural :: Type Natural
- [x] normalizer ::
  - (Only through dhall_add_builtin)
- ns pair :: Type a -> Type b -> Type (a, b)
- [ ] rawInput ::
- [x] record :: RecordType a -> Type a
- [ ] rootDirectory ::
- [ ] scientific :: Type scientific-0.3.6.2:Data.Scientific.Scientific
- ns Dhall.sequence :: Type a -> Type (Seq a)
- [ ] sourceName ::
- [ ] standardVersion ::
- [x] startingContext ::
  - (Only through dhall_add_builtin)
- [x]  strictText :: Type Text
- ns string :: Type String
- [x] unit :: Type ()
- [x] vector :: Type a -> Type (Vector a)
- [ ] Expr

ns = not supported

How to build
-----------------------------

Requirements:

- Haskell stack (https://docs.haskellstack.org/en/stable/README/)
- Boost build v2 (https://www.boost.org/doc/libs/1_50_0/tools/build/v2/index.html)

The following executes the test suite.

```
$ stack build
$ cd ctest
$ bjam
```

Installation for production is not yet implemented.