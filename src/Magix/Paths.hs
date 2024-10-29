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
    getLockPath,
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

getCommonPrefix :: FilePath -> Int -> String -> FilePath
getCommonPrefix cacheDir hash name =
  cacheDir
    </> showHash hash
      <> "-"
      <> map replaceProblematicChars name

getLockPath :: FilePath -> Int -> String -> FilePath
getLockPath cacheDir hash name = getCommonPrefix cacheDir hash name <> ".lock"

getScriptLinkPath :: FilePath -> Int -> String -> FilePath
getScriptLinkPath cacheDir hash name = getCommonPrefix cacheDir hash name <> "-script"

getBuildDir :: FilePath -> Int -> String -> FilePath
getBuildDir cacheDir hash name = getCommonPrefix cacheDir hash name <> "-build"

getBuildExprPath :: FilePath -> FilePath
getBuildExprPath buildDir = buildDir </> "default.nix"

getResultLinkPath :: FilePath -> Int -> String -> FilePath
getResultLinkPath cacheDir hash name = getCommonPrefix cacheDir hash name <> "-result"
