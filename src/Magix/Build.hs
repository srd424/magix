-- |
-- Module      :  Magix.Build
-- Description :  Build Magix expressions
-- Copyright   :  2024 Dominik Schrempf
-- License     :  GPL-3.0-or-later
--
-- Maintainer  :  dominik.schrempf@gmail.com
-- Stability   :  experimental
-- Portability :  not portable
--
-- Creation date: Sun Oct 20 09:48:50 2024.
module Magix.Build
  ( BuildStatus (..),
    getBuildStatus,
    build,
    removeBuild,
  )
where

import Control.Monad (when)
import Data.Text (Text)
import Data.Text.IO (writeFile)
import Magix.Config (Config (..))
import System.Directory
  ( createDirectoryIfMissing,
    doesDirectoryExist,
    doesFileExist,
    doesPathExist,
    removeDirectoryLink,
    removeDirectoryRecursive,
    removeFile,
  )
import System.Posix (createSymbolicLink)
import System.Process (callProcess)
import Prelude hiding (writeFile)

data BuildStatus = HasBeenBuilt | NeedToBuild

getBuildStatus :: Config -> IO BuildStatus
getBuildStatus c = do
  resultDirExists <- doesPathExist c.resultDir
  pure $ if resultDirExists then HasBeenBuilt else NeedToBuild

build :: Config -> Text -> IO ()
build c e = do
  -- Remove any previous builds.
  removeBuild c
  -- Make sure the cache directory exists.
  createDirectoryIfMissing True c.cacheDir
  -- Create sanitized link to script.
  createSymbolicLink c.scriptPath c.scriptLinkPath
  -- Build directory.
  createDirectoryIfMissing True c.buildDir
  -- Expression.
  let exprPath = buildExprPath c
  writeFile exprPath e
  -- Build.
  callProcess "nix-build" ["--out-link", c.resultDir, c.buildDir]

removeBuild :: Config -> IO ()
removeBuild c = do
  -- Link to script.
  scriptLinkPathExists <- doesFileExist c.scriptLinkPath
  when scriptLinkPathExists $ removeFile c.scriptLinkPath
  -- Build directory.
  buildDirExists <- doesDirectoryExist c.buildDir
  when buildDirExists $ removeDirectoryRecursive c.buildDir
  -- Result directory.
  resultDirExists <- doesDirectoryExist c.resultDir
  when resultDirExists $ removeDirectoryLink c.resultDir
