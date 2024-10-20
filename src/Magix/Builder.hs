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
module Magix.Builder (buildNixExpression) where

import Data.Text (Text)
import Magix.Config (MagixConfig)
import Magix.Haskell.Builder (buildHaskellNixExpression)
import Magix.Magix (Magix (..))
import Prelude hiding (unwords)

buildNixExpression :: MagixConfig -> Magix -> IO Text
buildNixExpression c (MHaskellMagix x) = buildHaskellNixExpression c x
