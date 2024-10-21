-- |
-- Module      :  Magix.Languages.Haskell.ExpressionSpec
-- Description :  Tests for building Haskell command lines
-- Copyright   :  2024 Dominik Schrempf
-- License     :  GPL-3.0-or-later
--
-- Maintainer  :  dominik.schrempf@gmail.com
-- Stability   :  experimental
-- Portability :  portable
--
-- Creation date: Fri Oct 18 15:52:10 2024.
module Magix.Languages.Haskell.ExpressionSpec
  ( spec,
  )
where

import Data.Text (Text)
import Magix.Config (Config (..))
import Magix.Languages.Haskell.Directives (HaskellDirectives (..))
import Magix.Languages.Haskell.Expression (getHaskellNixExpression)
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

haskellPackages :: [Text]
haskellPackages = ["fake", "packages"]

ghcFlags :: [Text]
ghcFlags = ["fake", "flags"]

directives :: HaskellDirectives
directives = HaskellDirectives haskellPackages ghcFlags

spec :: Spec
spec = do
  describe "getHaskellNixExpression" $ do
    it "works correctly for some sample data" $ do
      expr <- getHaskellNixExpression config directives
      expr `shouldSatisfy` doesNotContainTemplates
      expr `shouldSatisfy` containsSpaceSeparatedValues haskellPackages
      expr `shouldSatisfy` containsSpaceSeparatedValues ghcFlags
