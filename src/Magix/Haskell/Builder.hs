-- |
-- Module      :  Magix.Haskell.Builder
-- Description :  Build Haskell command lines
-- Copyright   :  2024 Dominik Schrempf
-- License     :  GPL-3.0-or-later
--
-- Maintainer  :  dominik.schrempf@gmail.com
-- Stability   :  experimental
-- Portability :  portable
--
-- Creation date: Fri Oct 18 13:36:32 2024.
module Magix.Haskell.Builder (buildHaskellNixExpression) where

import Data.Text (Text, pack, replace, unwords)
import Data.Text.IO (readFile)
import Magix.Config (MagixConfig (..))
import Magix.Haskell.Directives (HaskellMagix (..))
import Paths_magix (getDataFileName)
import Prelude hiding (readFile, unwords)

buildHaskellNixExpression :: MagixConfig -> HaskellMagix -> IO Text
buildHaskellNixExpression c (HaskellMagix ps fs) = do
  f <- getDataFileName "src/Magix/Haskell/Template.nix"
  e <- readFile f
  -- TODO: Foldl.
  let e' = replace "__SCRIPT_NAME__" (pack $ scriptName c) e
      e'' = replace "__SCRIPT_SOURCE__" (pack $ scriptPath c) e'
      e''' = replace "__HASKELL_PACKAGES__" (unwords ps) e''
      e'''' = replace "__HASKELL_GHC_FLAGS__" (unwords fs) e'''
  pure e''''
