-- |
-- Module      :  Magix.Tools
-- Description :  Helpers shared across tests
-- Copyright   :  2024 Dominik Schrempf
-- License     :  GPL-3.0-or-later
--
-- Maintainer  :  dominik.schrempf@gmail.com
-- Stability   :  experimental
-- Portability :  portable
--
-- Creation date: Mon Oct 21 21:56:26 2024.
module Magix.Tools
  ( doesNotContainTemplates,
    containsSpaceSeparatedValues,
    getFakeConfigWithHash,
  )
where

import Data.Text (Text, isInfixOf, unwords)
import Magix.Config (Config (..))
import Prelude hiding (unwords)

doesNotContainTemplates :: Text -> Bool
doesNotContainTemplates = not . isInfixOf "__"

containsSpaceSeparatedValues :: [Text] -> Text -> Bool
containsSpaceSeparatedValues xs = isInfixOf (unwords xs)

getFakeConfigWithHash :: Int -> Config
getFakeConfigWithHash h =
  Config
    "/tmp/fakeScriptPath"
    "/tmp/fakeScriptName"
    "/tmp/fakeNixpkgsPath"
    h
    "/tmp/fakeCacheDir"
    "/tmp/fakeScriptLinkPath"
    "/tmp/fakeBuildDir"
    "/tmp/fakeBuildExprPath"
    "/tmp/fakeResultDir"
