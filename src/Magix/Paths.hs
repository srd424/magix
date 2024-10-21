-- |
-- Module      :  Magix.Paths
-- Description :  Get paths to cache files
-- Copyright   :  2024 Dominik Schrempf
-- License     :  GPL-3.0-or-later
--
-- Maintainer  :  dominik.schrempf@gmail.com
-- Stability   :  experimental
-- Portability :  portable
--
-- Creation date: Mon Oct 21 17:06:03 2024.
module Magix.Paths
  ( getBuildDir,
    getExprPath,
    getResultDir,
    getLinkPath,
  )
where

import Data.Char (isAlphaNum)
import Magix.Config (Config (..))
import Numeric (showHex)
import System.Environment.XDG.BaseDir (getUserCacheFile)
import System.FilePath ((</>))

showHash :: Int -> String
showHash h
  | h < 0 = '1' : showHex (-h) ""
  | otherwise = '0' : showHex h ""

getBuildDir :: Config -> IO FilePath
getBuildDir (Config _ n h) = getUserCacheFile "magix" $ showHash h <> "-" <> n <> "-build"

getExprPath :: FilePath -> FilePath
getExprPath buildDir = buildDir </> "default.nix"

getResultDir :: Config -> IO FilePath
getResultDir (Config _ n h) = getUserCacheFile "magix" $ showHash h <> "-" <> n <> "-result"

replaceProblematicChars :: Char -> Char
replaceProblematicChars c
  | isAlphaNum c = c
  | otherwise = '-'

getLinkPath :: Config -> IO FilePath
getLinkPath (Config _ n h) =
  getUserCacheFile "magix" $
    showHash h
      <> "-"
      <> map replaceProblematicChars n
      <> "-script"
