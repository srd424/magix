-- |
-- Module      :  Magix.Languages.Bash.ExpressionSpec
-- Description :  Tests for building Bash command lines
-- Copyright   :  2024 Dominik Schrempf
-- License     :  GPL-3.0-or-later
--
-- Maintainer  :  dominik.schrempf@gmail.com
-- Stability   :  experimental
-- Portability :  portable
--
-- Creation date: Fri Oct 18 15:52:10 2024.
module Magix.Languages.Bash.ExpressionSpec
  ( spec,
  )
where

import Data.Text (Text)
import Magix.Config (Config (..))
import Magix.Languages.Bash.Directives (BashDirectives (..))
import Magix.Languages.Bash.Expression (getBashNixExpression)
import Magix.Languages.TestHelpers (containsSpaceSeparatedValues, doesNotContainTemplates)
import Test.Hspec (Spec, describe, it, shouldSatisfy)
import Prelude hiding (unwords)

config :: Config
config =
  Config
    "fakeScriptPath"
    "fakeScriptName"
    0
    "fakeCacheDir"
    "fakeScriptLinkPath"
    "fakeBuildDir"
    "fakeBuildExprPath"
    "fakeResultDir"

runtimeInputs :: [Text]
runtimeInputs = ["fake", "inputs"]

directives :: BashDirectives
directives = BashDirectives runtimeInputs

spec :: Spec
spec = do
  describe "getBashNixExpression" $ do
    it "works correctly for some sample data" $ do
      expr <- getBashNixExpression config directives
      expr `shouldSatisfy` doesNotContainTemplates
      expr `shouldSatisfy` containsSpaceSeparatedValues runtimeInputs
