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
  ( getScriptLinkPath,
    getBuildDir,
    getBuildExprPath,
    getResultLinkPath,
  )
where

import Data.Char (isAlphaNum)
import Numeric (showHex)
import System.FilePath ((</>))

showHash :: Int -> String
showHash hash
  | hash < 0 = '1' : showHex (-hash) ""
  | otherwise = '0' : showHex hash ""

replaceProblematicChars :: Char -> Char
replaceProblematicChars c
  | isAlphaNum c = c
  | otherwise = '-'

getScriptLinkPath :: FilePath -> String -> Int -> FilePath
getScriptLinkPath cacheDir name hash =
  cacheDir
    </> showHash hash
      <> "-"
      <> map replaceProblematicChars name
      <> "-script"

getBuildDir :: FilePath -> String -> Int -> FilePath
getBuildDir cacheDir name hash = cacheDir </> showHash hash <> "-" <> name <> "-build"

getBuildExprPath :: FilePath -> FilePath
getBuildExprPath buildDir = buildDir </> "default.nix"

getResultLinkPath :: FilePath -> String -> Int -> FilePath
getResultLinkPath cacheDir name hash = cacheDir </> showHash hash <> "-" <> name <> "-result"
