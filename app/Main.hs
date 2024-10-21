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

import Control.Monad (when)
import Data.Text (unpack)
import Data.Text.IO (readFile)
import Magix
  ( BuildStatus (..),
    Options (..),
    build,
    getBuildStatus,
    getConfig,
    getDirectives,
    getNixExpression,
    getOptions,
    removeBuild,
  )
import Magix.Options (Rebuild (..), Verbosity (..))
import Magix.Run (run)
import System.IO (Handle, stderr)
import System.Log.Formatter (simpleLogFormatter)
import System.Log.Handler (setFormatter)
import System.Log.Handler.Simple (GenericHandler, streamHandler)
import System.Log.Logger
  ( Logger,
    Priority (..),
    getLogger,
    logL,
    removeHandler,
    rootLoggerName,
    saveGlobalLogger,
    setHandlers,
    setLevel,
    updateGlobalLogger,
  )
import Prelude hiding (readFile)

withFormatter :: GenericHandler Handle -> GenericHandler Handle
withFormatter handler = setFormatter handler formatter
  where
    formatter = simpleLogFormatter "[$time $loggername $prio] $msg"

setupLogger :: Verbosity -> IO Logger
setupLogger v = do
  let prio = case v of
        Info -> INFO
        Debug -> DEBUG
  updateGlobalLogger rootLoggerName removeHandler
  stderrHandler <- withFormatter <$> streamHandler stderr prio
  logger <- setHandlers [stderrHandler] . setLevel prio <$> getLogger "Magix.Main"
  saveGlobalLogger logger
  pure logger

main :: IO ()
main = do
  opts <- getOptions
  logger <- setupLogger (verbosity opts)
  let logI = logL logger INFO
      logD = logL logger DEBUG
      logE = logL logger ERROR

  logD $ "Options are " <> show opts

  let p = scriptPath opts
  logD $ "Reading script at path " <> p
  f <- readFile p

  conf <- getConfig p f
  logD $ "Magix configuration is " <> show conf

  logD "Parsing directives"
  dirs <- case getDirectives p f of
    Left e -> logE "Failed parsing directives" >> error e
    Right ds -> pure ds
  logD $ "Directives are " <> show dirs

  when (rebuild opts == ForceRebuild) $ do
    logD "Removing existing build"
    removeBuild conf
    logD "Removed existing build"

  logD "Checking build status"
  buildStatus <- getBuildStatus conf
  case buildStatus of
    HasBeenBuilt -> logD "Script has already been built"
    NeedToBuild -> do
      logD "Need to build"
      logD "Getting Nix expression"
      expr <- getNixExpression conf dirs
      logD $ "Nix expression is " <> unpack expr
      logI "Building Nix expression"
      build conf expr
      logD "Built Nix expression"

  logD "Running"
  run opts conf
  logD "Done"
