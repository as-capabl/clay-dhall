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

import Foreign.C.String
import Foreign.Ptr
import Foreign.ForeignPtr
import Foreign.Storable
import Foreign.Marshal
import Foreign.StablePtr
import System.IO.Unsafe

import qualified Data.ByteString as B
import qualified Data.ByteString.Unsafe as B
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Vector as V

import Control.Lens

import qualified Dhall as Dh
import qualified Dhall.Core as DhC
import qualified Dhall.TypeCheck as DhTC
import qualified Dhall.Context as DhCtx

import Clay.Type
import Clay.Obj

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

foreign export ccall hsc_input :: CString -> Ptr CDhallTypedPtr -> IO Bool
hsc_input cs p =
  do
    !s <- T.decodeUtf8 <$> B.unsafePackCString cs

    t <- asHolderType p
    join $ Dh.input t s 
    
    return True

foreign export ccall hsc_input_with_settings :: StablePtr Obj -> CString -> Ptr CDhallTypedPtr -> IO Bool
hsc_input_with_settings spStg cs p =
  do
    !s <- T.decodeUtf8 <$> B.unsafePackCString cs
    stg <- objToInputSettings <$> deRefStablePtr spStg

    t <- asHolderType p
    join $ Dh.inputWithSettings stg t s 
    
    return True

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



