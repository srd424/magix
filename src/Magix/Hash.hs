-- |
-- Module      :  Magix.Hash
-- Description :  Create hashes of Magix configurations
-- Copyright   :  2024 Dominik Schrempf
-- License     :  GPL-3.0-or-later
--
-- Maintainer  :  dominik.schrempf@gmail.com
-- Stability   :  experimental
-- Portability :  portable
--
-- Creation date: Thu Oct 31 06:47:58 2024.
module Magix.Hash
  ( getMagixHash,
  )
where

import Data.Hashable (hash)
import Data.Text (Text)
import Paths_magix (version)

getMagixHash :: FilePath -> Text -> Int
getMagixHash nixpkgsPath scriptContents = hash (nixpkgsPath, version, scriptContents)
