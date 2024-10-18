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

import Data.Text.IO (readFile)
import Magix.Directives (pMagix)
import Options (MagixOptions (scriptFilePath), magixOptionsParser)
import Options.Applicative (execParser)
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
import System.OsPath (decodeUtf)
import Text.Megaparsec (errorBundlePretty, parse)
import Prelude hiding (readFile)

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

  logL logger DEBUG "Parsing options"
  opts <- execParser magixOptionsParser
  logL logger DEBUG $ "Parsed options are " <> show opts

  logL logger DEBUG "Parsing directives"
  let script = scriptFilePath opts
  path <- decodeUtf script
  contents <- readFile path
  let magix =
        either (error . errorBundlePretty) id $
          parse pMagix path contents
  logL logger DEBUG $ "Parsed directives are " <> show magix
