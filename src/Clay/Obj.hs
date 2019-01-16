{-# LANGUAGE UnboxedSums #-}
{-# LANGUAGE RankNTypes #-}

module
    Clay.Obj
where

import Foreign.Ptr

import qualified Dhall as Dh
import qualified Dhall.Core as DhC
import qualified Dhall.Parser as DhP
import qualified Dhall.TypeCheck as DhTC

import Control.Lens

--
-- Type-Erased 1-argument Function
--
type TEArg = Ptr ()
type TEDest = Ptr ()

-- |Type-Erased 1-argument Function
type TEFunc = TEArg -> TEDest -> IO ()

--
-- Managed object
--
data Obj = Obj
  (# 
    DhC.Expr DhP.Src DhTC.X |
    TEFunc |
    Dh.InputSettings |
    Dh.EvaluateSettings
  #)

mismatch :: String -> a
mismatch s = error $ "Object type mismatch: " ++ s ++ " expected."

-- Expr
objToExpr :: Obj -> DhC.Expr DhP.Src DhTC.X
objToExpr (Obj (# x | | | #)) = x
objToExpr _ = mismatch "Expr"

exprToObj :: (forall s. DhC.Expr DhP.Src DhTC.X) -> Obj
exprToObj x = Obj (# x | | | #)

-- TEFunc
objToTEFunc :: Obj -> TEFunc
objToTEFunc (Obj (# | x | | #)) = x
objToTEFunc _ = mismatch "TEFunc"

teFuncToObj :: TEFunc -> Obj
teFuncToObj x = Obj (# | x | | #)

-- InputSettings
objToInputSettings :: Obj -> Dh.InputSettings
objToInputSettings (Obj (# | | x | #)) = x
objToInputSettings _ = mismatch "InputSettings"

inputSettingsToObj :: Dh.InputSettings -> Obj
inputSettingsToObj x = Obj (# | | x | #)


-- EvaluateSettings
objToEvaluateSettings :: Obj -> Dh.EvaluateSettings
objToEvaluateSettings (Obj (# | | | x #)) = x
objToEvaluateSettings _ = mismatch "EvaluateSettings"

evaluateSettingsToObj :: Dh.EvaluateSettings -> Obj
evaluateSettingsToObj x = Obj (# | | | x #)


-- Misc
applyES :: (forall a. Dh.HasEvaluateSettings a => a -> a) -> Obj -> Obj
applyES f (Obj (# | | x | #)) = Obj (# | | f x | #)
applyES f (Obj (# | | | x #)) = Obj (# | | | f x #)
applyES _ _ = mismatch "InputSettings | EvaluateSettings"