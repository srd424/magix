-- |
-- Module      :  Magix.ExpressionSpec
-- Description :  Tests for building Haskell command lines
-- Copyright   :  2024 Dominik Schrempf
-- License     :  GPL-3.0-or-later
--
-- Maintainer  :  dominik.schrempf@gmail.com
-- Stability   :  experimental
-- Portability :  portable
--
-- Creation date: Fri Oct 18 15:52:10 2024.
module Magix.ExpressionSpec
  ( spec,
  )
where

import Data.Text (Text, isInfixOf)
import Magix.Config (Config (..))
import Magix.Directives (Directives (..))
import Magix.Expression (getNixExpression, getReplacements, getTemplate)
import Magix.Languages.Bash.Directives (BashDirectives (..))
import Magix.Languages.Haskell.Directives (HaskellDirectives (..))
import Magix.Tools (containsSpaceSeparatedValues, doesNotContainTemplates)
import Test.Hspec (Spec, describe, it, shouldBe, shouldSatisfy)
import Prelude hiding (unwords)

allReplacementsUsed :: Text -> [(Text, Text)] -> Bool
allReplacementsUsed x = all (\(r, _) -> r `isInfixOf` x)

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

bashDirectives :: BashDirectives
bashDirectives = BashDirectives runtimeInputs

haskellPackages :: [Text]
haskellPackages = ["fake", "packages"]

ghcFlags :: [Text]
ghcFlags = ["fake", "flags"]

haskellDirectives :: HaskellDirectives
haskellDirectives = HaskellDirectives haskellPackages ghcFlags

spec :: Spec
spec = do
  describe "replacements" $ do
    it "all replacements should be used" $ do
      let bds = Bash bashDirectives
      bashTempl <- getTemplate bds
      allReplacementsUsed bashTempl (getReplacements config bds) `shouldBe` True

      let hds = Haskell haskellDirectives
      haskellTempl <- getTemplate hds
      allReplacementsUsed haskellTempl (getReplacements config hds) `shouldBe` True

  describe "getNixExpression" $ do
    it "works correctly for some sample data" $ do
      bashExpr <- getNixExpression config (Bash bashDirectives)
      bashExpr `shouldSatisfy` doesNotContainTemplates
      bashExpr `shouldSatisfy` containsSpaceSeparatedValues runtimeInputs

      haskellExpr <- getNixExpression config (Haskell haskellDirectives)
      haskellExpr `shouldSatisfy` doesNotContainTemplates
      haskellExpr `shouldSatisfy` containsSpaceSeparatedValues haskellPackages
      haskellExpr `shouldSatisfy` containsSpaceSeparatedValues ghcFlags
