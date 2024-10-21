-- |
-- Module      :  Magix.Build
-- Description :  Build Magix expressions
-- Copyright   :  2024 Dominik Schrempf
-- License     :  GPL-3.0-or-later
--
-- Maintainer  :  dominik.schrempf@gmail.com
-- Stability   :  experimental
-- Portability :  portable
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
import Magix.Paths (getBuildDir, getExprPath, getResultDir)
import System.Directory
  ( createDirectoryIfMissing,
    doesDirectoryExist,
    doesPathExist,
    removeDirectoryLink,
    removeDirectoryRecursive,
  )
import System.Process (callProcess)
import Prelude hiding (writeFile)

data BuildStatus = HasBeenBuilt | NeedToBuild

getBuildStatus :: Config -> IO BuildStatus
getBuildStatus c = do
  resultDir <- getResultDir c
  resultDirExists <- doesPathExist resultDir
  pure $ if resultDirExists then HasBeenBuilt else NeedToBuild

build :: Config -> Text -> IO ()
build c e = do
  -- Build directory.
  buildDir <- getBuildDir c
  createDirectoryIfMissing True buildDir
  -- Expression.
  let exprPath = getExprPath buildDir
  writeFile exprPath e
  -- Build.
  resultDir <- getResultDir c
  callProcess "nix-build" ["--out-link", resultDir, buildDir]

removeBuild :: Config -> IO ()
removeBuild c = do
  -- Build directory.
  buildDir <- getBuildDir c
  buildDirExists <- doesDirectoryExist buildDir
  when buildDirExists $ removeDirectoryRecursive buildDir
  -- Result directory.
  resultDir <- getResultDir c
  resultDirExists <- doesDirectoryExist resultDir
  when resultDirExists $ removeDirectoryLink resultDir
