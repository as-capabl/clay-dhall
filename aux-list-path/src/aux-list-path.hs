{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}

module
    Main
where

import Control.Monad (guard)    
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Text.Lazy.Builder (toLazyText)
import Data.Text.Lazy (toStrict)
import qualified Data.Map.Strict as Map
import System.Environment (getArgs)
import System.Exit (die)
import Cabal.Plan

main :: IO ()
main =
  do
    args <- getArgs
    if length args < 2 then die_ usageStr else return ()
    let [jsonPath, action] = args

    PlanJson {pjUnits} <- decodePlanJson jsonPath

    unit <-
      do
        let l = 
              do
                (_, u)   <- Map.toList pjUnits
                (cn, _) <- Map.toList $ uComps u
                guard $ cn == CompNameFLib "clay-dhall"
                return u 
        case l
          of
            u:_ -> return u
            []  -> die_ "No configured foreign library `clay-dhall`"
            
    case action
      of
        "dist-dir" ->
            maybe
                (die_ $ "`clay-dhall` isn't a local package." )
                putStrLn $
                    uDistDir unit
        _ ->
            die_ $ "Unknown action `" <> T.pack action <> "`\n"

die_ :: T.Text -> IO a
die_ = die . T.unpack

usageStr :: T.Text
usageStr = toStrict . toLazyText . mconcat $ [
    "usage: aux-list-path <path to plan.json> <action>\n",
    "  actions\n",
    "  - dist-dir: show dist directory\n"
  ]
