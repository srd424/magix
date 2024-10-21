-- |
-- Module      :  Magix.Languages.TestHelpers
-- Description :  Helpers shared across tests
-- Copyright   :  2024 Dominik Schrempf
-- License     :  GPL-3.0-or-later
--
-- Maintainer  :  dominik.schrempf@gmail.com
-- Stability   :  experimental
-- Portability :  portable
--
-- Creation date: Mon Oct 21 21:56:26 2024.
module Magix.Languages.TestHelpers
  ( doesNotContainTemplates,
    containsSpaceSeparatedValues,
  )
where

import Data.Text (Text, isInfixOf, unwords)
import Prelude hiding (unwords)

doesNotContainTemplates :: Text -> Bool
doesNotContainTemplates = not . isInfixOf "__"

containsSpaceSeparatedValues :: [Text] -> Text -> Bool
containsSpaceSeparatedValues xs = isInfixOf (unwords xs)
