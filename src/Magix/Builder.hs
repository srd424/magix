-- |
-- Module      :  Magix.Builder
-- Description :  Build `nix-script` command line
-- Copyright   :  2024 Dominik Schrempf
-- License     :  GPL-3.0-or-later
--
-- Maintainer  :  dominik.schrempf@gmail.com
-- Stability   :  experimental
-- Portability :  portable
--
-- Creation date: Fri Oct 18 13:36:32 2024.
module Magix.Builder (buildArgs) where

import Magix.Haskell.Builder (buildHaskellArgs)
import Magix.Magix (Magix (..))
import Magix.Options (MagixOptions)
import Prelude hiding (unwords)

buildArgs :: MagixOptions -> Magix -> [String]
buildArgs o (MHaskellMagix x) = buildHaskellArgs o x
