-- |
-- Module      :  Magix
-- Description :  Spine of Magix
-- Copyright   :  2024 Dominik Schrempf
-- License     :  GPL-3.0-or-later
--
-- Maintainer  :  dominik.schrempf@gmail.com
-- Stability   :  experimental
-- Portability :  portable
--
-- Creation date: Fri Oct 18 09:12:29 2024.
module Magix
  ( Magix (..),
  )
where

import Data.Text (Text)

newtype Magix = HMagix {haskellPackages :: [Text]}
  deriving (Eq, Show)
