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
-- Creation date: Sun Oct 20 08:35:31 2024.
module Magix.Config
  ( MagixConfig (..),
    getMagixConfig,
  )
where

import Magix.Options (MagixOptions (MagixOptions))
import System.FilePath (takeBaseName)

data MagixConfig = MagixConfig
  { scriptPath :: !FilePath,
    scriptName :: !String
  }
  deriving (Eq, Show)

getMagixConfig :: MagixOptions -> MagixConfig
getMagixConfig (MagixOptions _ p) = MagixConfig p (takeBaseName p)
