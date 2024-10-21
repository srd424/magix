-- |
-- Module      :  Magix.Config
-- Description :  Magix configuration
-- Copyright   :  2024 Dominik Schrempf
-- License     :  GPL-3.0-or-later
--
-- Maintainer  :  dominik.schrempf@gmail.com
-- Stability   :  experimental
-- Portability :  portable
--
-- Creation date: Fri Oct 18 09:12:29 2024.
module Magix.Config
  ( Config (..),
    getConfig,
  )
where

import Data.Hashable (hash)
import Data.Text (Text)
import Magix.Paths (getBuildDir, getBuildExprPath, getResultDir, getScriptLinkPath)
import System.Directory (canonicalizePath)
import System.Environment.XDG.BaseDir (getUserCacheDir)
import System.FilePath (takeBaseName)
import Prelude hiding (readFile)

data Config = Config
  { scriptPath :: !FilePath,
    scriptName :: !String,
    scriptHash :: !Int,
    -- | Cache directory containing Nix expressions and build results.
    cacheDir :: !FilePath,
    -- | Sanitized path to the link to the original script.
    scriptLinkPath :: !FilePath,
    -- | Directory containing the Nix expression building the result.
    buildDir :: !FilePath,
    -- | The Nix expression building the result.
    buildExprPath :: !FilePath,
    -- | Directory containing the result of the build.
    resultDir :: !FilePath
  }
  deriving (Eq, Show)

getConfig :: FilePath -> Text -> IO Config
getConfig p x = do
  p' <- canonicalizePath p
  c <- getUserCacheDir "magix"
  let n = takeBaseName p
      h = hash x
      l = getScriptLinkPath c n h
      d = getBuildDir c n h
      e = getBuildExprPath d
      r = getResultDir c n h
  pure $ Config p' n h c l d e r
