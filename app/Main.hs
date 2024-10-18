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

import Magix (MagixOptions (..), getDirectives, getMagixOptions, runMagix)
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
  updateGlobalLogger rootLoggerName removeHandler
  stderrHandler <- withFormatter <$> streamHandler stderr DEBUG
  logger <- setHandlers [stderrHandler] . setLevel DEBUG <$> getLogger "Magix.Main"
  saveGlobalLogger logger
  let logD = logL logger DEBUG

  logD "Parsing options"
  opts <- getMagixOptions
  logD $ "Options are " <> show opts

  logD "Parsing directives"
  magix <- getDirectives $ scriptPath opts
  logD $ "Directives are " <> show magix

  logD "Running script"
  runMagix opts magix
  logD "Done"
