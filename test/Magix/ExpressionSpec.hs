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
import Magix.Config (Config)
import Magix.Directives (Directives (..), getLanguageName)
import Magix.Expression (getNixExpression, getReplacements, getTemplate)
import Magix.Languages.Bash.Directives (BashDirectives (..))
import Magix.Languages.Haskell.Directives (HaskellDirectives (..))
import Magix.Tools (containsSpaceSeparatedValues, doesNotContainTemplates, getFakeConfigWithHash)
import Test.Hspec (Spec, describe, it, shouldBe, shouldSatisfy)
import Prelude hiding (unwords)

allReplacementsUsed :: Text -> [(Text, Text)] -> Bool
allReplacementsUsed x = all (\(r, _) -> r `isInfixOf` x)

config :: Config
config = getFakeConfigWithHash 0

runtimeInputs :: [Text]
runtimeInputs = ["fake", "inputs"]

bashDirectives :: Directives
bashDirectives = Bash $ BashDirectives runtimeInputs

haskellPackages :: [Text]
haskellPackages = ["fake", "packages"]

ghcFlags :: [Text]
ghcFlags = ["fake", "flags"]

haskellDirectives :: Directives
haskellDirectives = Haskell $ HaskellDirectives haskellPackages ghcFlags

spec :: Spec
spec = do
  describe "getReplacements" $ do
    it "all replacements should be used as placeholders in the templates" $ do
      bashTempl <- getTemplate $ getLanguageName bashDirectives
      allReplacementsUsed bashTempl (getReplacements config bashDirectives) `shouldBe` True

      haskellTempl <- getTemplate $ getLanguageName haskellDirectives
      allReplacementsUsed haskellTempl (getReplacements config haskellDirectives) `shouldBe` True

  describe "getNixExpression" $ do
    it "all placeholders in templates should be replaced" $ do
      bashExpr <- getNixExpression config bashDirectives
      bashExpr `shouldSatisfy` doesNotContainTemplates

      haskellExpr <- getNixExpression config haskellDirectives
      haskellExpr `shouldSatisfy` doesNotContainTemplates

  describe "getNixExpression" $ do
    it "works correctly for some sample data" $ do
      bashExpr <- getNixExpression config bashDirectives
      bashExpr `shouldSatisfy` containsSpaceSeparatedValues runtimeInputs

      haskellExpr <- getNixExpression config haskellDirectives
      haskellExpr `shouldSatisfy` containsSpaceSeparatedValues haskellPackages
      haskellExpr `shouldSatisfy` containsSpaceSeparatedValues ghcFlags
