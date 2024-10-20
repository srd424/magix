-- |
-- Module      :  Magix.Haskell.BuilderSpec
-- Description :  Tests for building Haskell command lines
-- Copyright   :  2024 Dominik Schrempf
-- License     :  GPL-3.0-or-later
--
-- Maintainer  :  dominik.schrempf@gmail.com
-- Stability   :  experimental
-- Portability :  portable
--
-- Creation date: Fri Oct 18 15:52:10 2024.
module Magix.Haskell.BuilderSpec
  ( spec,
  )
where

import Data.Text (Text, isInfixOf)
import Magix.Config (Config (..))
import Magix.Haskell.Builder (buildHaskellNixExpression)
import Magix.Haskell.Directives (HaskellDirectives (..))
import Test.Hspec (Spec, describe, it, shouldSatisfy)

config :: Config
config = Config "fakeScriptPath" "fakeScriptName" 0

directives :: HaskellDirectives
directives = HaskellDirectives ["fake", "packages"] ["fake", "flags"]

-- NOTE: Here we could be a bit more rigorous and parse proper templates.
doesNotContainTemplate :: Text -> Bool
doesNotContainTemplate = not . isInfixOf "__"

spec :: Spec
spec = do
  describe "buildHaskellNixExpression" $ do
    it "works correctly for some sample data" $ do
      expr <- buildHaskellNixExpression config directives
      expr `shouldSatisfy` doesNotContainTemplate
