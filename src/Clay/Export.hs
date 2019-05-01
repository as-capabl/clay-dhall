{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module
    Clay.Export
where

import Control.Monad (join)
import Control.Monad.State
import Control.Monad.Identity
import Control.Monad.IO.Class

import Foreign.C.Types (CChar)
import Foreign.C.String
import Foreign.Ptr
import Foreign.ForeignPtr
import Foreign.ForeignPtr.Unsafe
import Foreign.Storable
import Foreign.Marshal
import Foreign.StablePtr
import System.IO.Unsafe
import qualified Control.Exception as Ex

import qualified Data.ByteString as B
import qualified Data.ByteString.Unsafe as B
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Vector as V

import Control.Lens

import qualified Dhall as Dh
import qualified Dhall.Core as DhC
import qualified Dhall.Parser as DhP
import qualified Dhall.TypeCheck as DhTC
import qualified Dhall.Context as DhCtx

import qualified Data.Text.Prettyprint.Doc as PP
import qualified Data.Text.Prettyprint.Doc.Render.Text as PP

import Clay.Type
import Clay.Obj


data ErrorInfo = ErrorInfo {
    getErrorNo :: ErrorCode,
    getErrorStr :: ForeignPtr CChar
  }

foreign import ccall "&g_lasterror" gLastError :: Ptr (StablePtr ErrorInfo)

exceptionGuard :: (IO ()) -> IO Bool
exceptionGuard act =
    (act >> return True) `Ex.catches` handlers
  where
    handlers = [
        Ex.Handler $ \e -> putE eCDHALL_ERROR_INVALID_TYPE (e :: Dh.InvalidType),
        Ex.Handler $ \e -> putE eCDHALL_ERROR_INVALID_TYPE (e :: DhTC.TypeError DhP.Src DhTC.X),
        Ex.Handler $ \e -> putE (arithErrorCode e) (e :: Ex.ArithException)
      ]
    putE :: Ex.Exception e => ErrorCode -> e -> IO Bool  
    putE n e =
      do
        cleanLastError
        frgn <- foreignCStr $ Ex.displayException e
        sptr <- newStablePtr $ ErrorInfo n frgn
        poke gLastError sptr
        return False
    cleanLastError =
      do
        sptr  <- peek gLastError
        if sptr /= castPtrToStablePtr nullPtr
            then freeStablePtr sptr
            else return ()
    foreignCStr str =
      do
        cstr <- newCString str
        newForeignPtr finalizerFree cstr

    arithErrorCode Ex.Overflow = eCDHALL_ERROR_ARITH_OVERFLOW
    arithErrorCode Ex.Underflow = eCDHALL_ERROR_ARITH_UNDERFLOW
    arithErrorCode Ex.LossOfPrecision = eCDHALL_ERROR_ARITH_LOSS_OF_PRECISION 
    arithErrorCode Ex.DivideByZero = eCDHALL_ERROR_ARITH_DIVIDE_BY_ZERO 
    arithErrorCode Ex.Denormal = eCDHALL_ERROR_ARITH_DENORMAL 
    arithErrorCode Ex.RatioZeroDenominator = eCDHALL_ERROR_ARITH_RATIO_ZERO_DENOMINATOR

getErrorInfo :: IO (Maybe ErrorInfo)
getErrorInfo =
  do
    sptr <- peek gLastError
    if sptr /= castPtrToStablePtr nullPtr
        then Just <$> deRefStablePtr sptr
        else return Nothing

foreign export ccall hsc_last_error_code :: IO ErrorCode
hsc_last_error_code =
  do
    mei <- getErrorInfo
    return $
        maybe eCDHALL_ERROR_EMPTY getErrorNo mei

foreign export ccall hsc_last_error_message :: IO CString
hsc_last_error_message =
  do
    mei <- getErrorInfo
    return $
        maybe nullPtr (unsafeForeignPtrToPtr . getErrorStr) mei
    
--
-- Exported functions
--
foreign export ccall hsc_free_array :: Ptr () -> IO ()
hsc_free_array p = free p

foreign export ccall hsc_clone_object :: StablePtr Obj -> IO (StablePtr Obj)
hsc_clone_object p =
  do
    v <- deRefStablePtr p
    newStablePtr v

foreign export ccall hsc_free_object :: StablePtr Obj -> IO ()
hsc_free_object p = freeStablePtr p

foreign export ccall hsc_call_func :: StablePtr Obj -> TEArg -> TEDest -> IO Bool
hsc_call_func p arg dest =
  do
    f <- objToTEFunc <$> deRefStablePtr p
    f arg dest
    return True

foreign export ccall hsc_show_expr_simple :: StablePtr Obj -> IO CString
hsc_show_expr_simple p =
  do
    expr <- objToExpr <$> deRefStablePtr p
    let stream = PP.layoutCompact (PP.pretty expr)
        txt = PP.renderStrict stream
        bs = T.encodeUtf8 txt
    allocCopyCS bs


foreign export ccall hsc_input :: CString -> Ptr CDhallTypedPtr -> IO Bool
hsc_input cs p =
  do
    !s <- T.decodeUtf8 <$> B.unsafePackCString cs

    t <- asHolderType p
    exceptionGuard $
        join $ Dh.input t s 

foreign export ccall hsc_input_with_settings :: StablePtr Obj -> CString -> Ptr CDhallTypedPtr -> IO Bool
hsc_input_with_settings spStg cs p =
  do
    !s <- T.decodeUtf8 <$> B.unsafePackCString cs
    stg <- objToInputSettings <$> deRefStablePtr spStg

    t <- asHolderType p
    exceptionGuard $
        join $ Dh.inputWithSettings stg t s 


foreign export ccall hsc_input_file :: CString -> Ptr CDhallTypedPtr -> IO Bool
hsc_input_file cs p =
    do
    !s <- T.decodeUtf8 <$> B.unsafePackCString cs

    t <- asHolderType p
    exceptionGuard $
        join $ Dh.inputFile t (T.unpack s)


foreign export ccall hsc_input_file_with_settings :: StablePtr Obj -> CString -> Ptr CDhallTypedPtr -> IO Bool
hsc_input_file_with_settings spStg cs p =
    do
    !s <- T.decodeUtf8 <$> B.unsafePackCString cs
    stg <- objToEvaluateSettings <$> deRefStablePtr spStg

    t <- asHolderType p
    exceptionGuard $
        join $ Dh.inputFileWithSettings stg t (T.unpack s)


foreign export ccall hsc_input_expr :: CString -> IO (StablePtr Obj)
hsc_input_expr cs =
  do
    !s <- T.decodeUtf8 <$> B.unsafePackCString cs
    r <- Dh.inputExpr s
    newStablePtr (exprToObj r)

foreign export ccall hsc_extract :: StablePtr Obj -> Ptr CDhallTypedPtr -> IO Bool
hsc_extract pE p =
  do
    e <- objToExpr <$> deRefStablePtr pE

    t <- asHolderType p
    maybe (return False) (>> return True) $ Dh.extract t e 

foreign export ccall hsc_embed :: Ptr CDhallTypedPtr -> IO (StablePtr Obj)
hsc_embed p =
  do
    CDhallTypedPtr{..} <- peekTypedPtr p
    CDhallTypeHolder{..} <- typeSpecBy tptrSpec

    it <- thPeek tptrPtr 
    newStablePtr . exprToObj $ Dh.embed it ()

foreign export ccall hsc_expr_eq :: StablePtr Obj -> StablePtr Obj -> IO Bool
hsc_expr_eq x y =
  do
    xExpr <- objToExpr <$> deRefStablePtr x
    yExpr <- objToExpr <$> deRefStablePtr y
    return $ xExpr == yExpr

{-
instance Hashable (DhC.Expr DhP.Src DhTC.X) where {}
instance Hashable (DhC.Binding DhP.Src DhTC.X) where {}
instance Hashable (DhC.Chunks DhP.Src DhTC.X) where {}
instance Hashable (DhMap.Map Dh.Text (DhC.Expr DhP.Src DhTC.X)) where {}
foreign export ccall hsc_expr_hash :: StablePtr Obj -> IO Int
hsc_expr_hash x =
  do
    xExpr <- objToExpr <$> deRefStablePtr x
    return $ hash xExpr
-}

--
-- Settings
--
sptrUpdate :: (a -> a) -> Ptr (StablePtr a) -> IO () 
sptrUpdate f p =
  do
    pStb <- peek p
    x <- deRefStablePtr pStb
    freeStablePtr pStb
    pStb' <- newStablePtr $! f x
    poke p pStb'

objSet :: Lens' Obj a -> Ptr (StablePtr Obj) -> a -> IO ()
objSet ln p x =
  do
    pStb <- peek p
    o <- deRefStablePtr pStb
    freeStablePtr pStb
    pStb' <- newStablePtr $! o & ln .~ x
    poke p pStb'

foreign export ccall hsc_new_evaluate_settings :: IO (StablePtr Obj)
hsc_new_evaluate_settings = newStablePtr $ evaluateSettingsToObj Dh.defaultEvaluateSettings

foreign export ccall hsc_new_input_settings :: IO (StablePtr Obj)
hsc_new_input_settings = newStablePtr $ inputSettingsToObj Dh.defaultInputSettings


type UserData = ()
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
hsc_add_builtin stg csName nArg pArgPtr resPtr fin pUData evalPtr =
  do
    !name <- T.decodeUtf8 <$> B.unsafePackCString csName
    args <- typeSpecByN nArg pArgPtr
    res <- typeSpecBy resPtr
    pForeign <- if pUData /= nullPtr then newForeignPtr fin pUData else newForeignPtr_ pUData

    let -- Context
        typePushArg arg = DhC.Pi "_" (Dh.expected $ thPoke arg)
        ctxBody = V.foldr typePushArg (Dh.expected $ thPoke res) args

        -- Normalizer
        eval = derefBuiltinImpl evalPtr

        nmrzPopArg i arg = StateT $ \case
            Just (DhC.App rest x, act) ->
              do
                let act' = \p ->
                      do
                        sp <- newStablePtr $! exprToObj $ DhC.denote x
                        pokeElemOff p (fromIntegral i) sp
                        act p
                return ((), Just (rest, act'))
            Nothing ->
                return ((), Nothing)

        nmrz :: DhC.Expr s DhTC.X -> Maybe (DhC.Expr s DhTC.X)
        nmrz expr = unsafePerformIO $
          do
            mx <- execStateT `flip` (Just (expr, \_ -> return ())) $
                V.imapM_ nmrzPopArg args
            case mx
              of
                Just (DhC.Var name, act) -> liftIO $
                    allocaBytes (sizeOf (undefined :: StablePtr Obj) * fromIntegral nArg) $ \pArgArray ->
                    withForeignPtr pForeign $ \pUData' ->
                      do
                        act pArgArray
                        spObj <- eval pUData' pArgArray
                        o <- deRefStablePtr spObj
                        freeStablePtr spObj
                        forM_ [0 .. nArg-1] $ \i ->
                          do
                            sp <- peekElemOff pArgArray (fromIntegral i)
                            freeStablePtr sp
                        return . Just $! DhC.denote $ objToExpr o
                Nothing -> return Nothing

    sptrUpdate `flip` stg $ applyES $ execState $
      do
        Dh.startingContext <%= DhCtx.insert name ctxBody
        Dh.normalizer <%= \(DhC.ReifiedNormalizer nm) -> DhC.ReifiedNormalizer $ \expr ->
            Identity $ case runIdentity $ nm expr
              of
                Just expr2 -> Just $ maybe expr2 id $ nmrz expr2
                Nothing -> nmrz expr



