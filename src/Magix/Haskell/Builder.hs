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
module Magix.Haskell.Builder (buildHaskellArgs) where

import Data.Text (Text, unpack, unwords)
import Magix.Haskell.Directives (HaskellMagix (..))
import Magix.Options (MagixOptions (..))
import Prelude hiding (unwords)

buildHaskellArgs :: MagixOptions -> HaskellMagix -> [String]
buildHaskellArgs o (HaskellMagix ps fs) =
  map unpack (buildCommandArgs ++ buildInputArgs)
    ++ [scriptPath o]
  where
    buildCommandArgs = ["--build-command", getBuildCommand fs]
    buildInputArgs = ["--build-input", getBuildInput ps]

-- 'mv $SRC $SRC.hs; ghc -threaded -o $OUT $SRC.hs'
getBuildCommand :: [Text] -> Text
getBuildCommand ghcFlags =
  "mv $SRC $SRC.hs; ghc "
    <> unwords ghcFlags
    <> " -o $OUT $SRC.hs"

-- 'haskellPackages.ghcWithPackages (ps: with ps; [ containers text turtle ])'
getBuildInput :: [Text] -> Text
getBuildInput haskellPackages =
  "haskellPackages.ghcWithPackages (ps: with ps; [ "
    <> unwords haskellPackages
    <> " ])"
