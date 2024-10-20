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
import Magix.Config (MagixConfig (..))
import Magix.Haskell.Builder (buildHaskellNixExpression)
import Magix.Haskell.Directives (HaskellMagix (..))
import Test.Hspec (Spec, describe, it, shouldSatisfy)

magixConfig :: MagixConfig
magixConfig = MagixConfig "fakeScriptPath" "fakeScriptName"

magix :: HaskellMagix
magix = HaskellMagix ["fake", "packages"] ["fake", "flags"]

-- NOTE: Here we could be a bit more rigorous and parse proper templates.
doesNotContainTemplate :: Text -> Bool
doesNotContainTemplate = not . isInfixOf "__"

spec :: Spec
spec = do
  describe "buildHaskellNixExpression" $ do
    it "works correctly for some sample data" $ do
      expr <- buildHaskellNixExpression magixConfig magix
      expr `shouldSatisfy` doesNotContainTemplate
