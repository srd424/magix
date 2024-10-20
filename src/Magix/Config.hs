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
-- Creation date: Fri Oct 18 09:12:29 2024.
module Magix.Config
  ( Config (..),
    getConfig,
  )
where

import Data.Hashable (hash)
import Data.Text (Text)
import System.Directory (makeAbsolute)
import System.FilePath (takeBaseName)
import Prelude hiding (readFile)

data Config = Config
  { scriptPath :: !FilePath,
    scriptName :: !String,
    scriptHash :: !Int
  }
  deriving (Eq, Show)

getConfig :: FilePath -> Text -> IO Config
getConfig p x = do
  absP <- makeAbsolute p
  pure $ Config absP (takeBaseName p) (hash x)
