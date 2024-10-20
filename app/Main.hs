-- |
-- Module      :  Main
-- Description :  Magically run and cache compiled scripts
-- Copyright   :  2024 Dominik Schrempf
-- License     :  GPL-3.0-or-later
--
-- Maintainer  :  dominik.schrempf@gmail.com
-- Stability   :  experimental
-- Portability :  portable
--
-- Creation date: Fri Oct 18 09:12:06 2024.
module Main
  ( main,
  )
where

import Data.Text (unpack)
import Magix
  ( MagixOptions (..),
    buildNixExpression,
    getDirectives,
    getMagixConfig,
    getMagixOptions,
  )
import Magix.Options (Verbosity (..))
import System.IO (Handle, stderr)
import System.Log.Formatter (simpleLogFormatter)
import System.Log.Handler (setFormatter)
import System.Log.Handler.Simple (GenericHandler, streamHandler)
import System.Log.Logger
  ( Priority (..),
    getLogger,
    logL,
    removeHandler,
    rootLoggerName,
    saveGlobalLogger,
    setHandlers,
    setLevel,
    updateGlobalLogger,
  )

withFormatter :: GenericHandler Handle -> GenericHandler Handle
withFormatter handler = setFormatter handler formatter
  where
    formatter = simpleLogFormatter "[$time $loggername $prio] $msg"

main :: IO ()
main = do
  opts <- getMagixOptions

  let prio = case verbosity opts of
        Info -> INFO
        Debug -> DEBUG
  updateGlobalLogger rootLoggerName removeHandler
  stderrHandler <- withFormatter <$> streamHandler stderr prio
  logger <- setHandlers [stderrHandler] . setLevel prio <$> getLogger "Magix.Main"
  saveGlobalLogger logger
  let logD = logL logger DEBUG

  logD $ "Options are " <> show opts

  let conf = getMagixConfig opts
  logD $ "Magix configuration is " <> show conf

  logD "Parsing directives"
  magix <- getDirectives $ scriptPath opts
  logD $ "Directives are " <> show magix

  logD "Building Nix expression"
  expr <- buildNixExpression conf magix
  logD $ "Built Nix expression is " <> unpack expr
