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
import Magix.NixpkgsPath (getDefaultNixpkgsPath)
import Magix.Options (Options (..))
import Magix.Paths (getBuildDir, getBuildExprPath, getResultLinkPath, getScriptLinkPath)
import System.Directory (canonicalizePath)
import System.Environment.XDG.BaseDir (getUserCacheDir)
import System.FilePath (takeBaseName)
import Prelude hiding (readFile)

data Config = Config
  { scriptPath :: !FilePath,
    scriptName :: !String,
    -- | The Magix hash includes the hash of the script directory and the path
    -- of the Nixpkgs directory.
    nixpkgsPath :: !FilePath,
    magixHash :: !Int,
    -- | Cache directory containing Nix expressions and build results.
    cacheDir :: !FilePath,
    -- | Sanitized path of the link to the original script.
    scriptLinkPath :: !FilePath,
    -- | Directory containing the Nix expression building the result.
    buildDir :: !FilePath,
    -- | The Nix expression building the result.
    buildExprPath :: !FilePath,
    -- | Link to directory containing the result of the build.
    resultLinkPath :: !FilePath
  }
  deriving (Eq, Show)

getDefaultNixpkgsPathOrFail :: IO FilePath
getDefaultNixpkgsPathOrFail = do
  mr <- getDefaultNixpkgsPath
  case mr of
    Left err -> do
      putStrLn "Could not retrieve Nixpkgs path from NIX_PATH"
      error err
    Right np -> pure np

getConfig :: Options -> Text -> IO Config
getConfig o x = do
  p' <- canonicalizePath p
  c <- maybe (getUserCacheDir "magix") canonicalizePath o.cachePath
  np <- maybe getDefaultNixpkgsPathOrFail canonicalizePath o.nixpkgsPath
  let nm = takeBaseName p
      ha = hash (x, np)
      lp = getScriptLinkPath c nm ha
      bd = getBuildDir c nm ha
      be = getBuildExprPath bd
      rd = getResultLinkPath c nm ha
  pure $
    Config
      { scriptPath = p',
        scriptName = nm,
        magixHash = ha,
        nixpkgsPath = np,
        cacheDir = c,
        scriptLinkPath = lp,
        buildDir = bd,
        buildExprPath = be,
        resultLinkPath = rd
      }
  where
    p = o.scriptPath
