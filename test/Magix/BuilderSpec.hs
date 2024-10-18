-- |
-- Module      :  Magix.BuilderSpec
-- Description :  Unit tests for BuilderSpec
-- Copyright   :  2024 Dominik Schrempf
-- License     :  GPL-3.0-or-later
--
-- Maintainer  :  dominik.schrempf@gmail.com
-- Stability   :  experimental
-- Portability :  portable
--
-- Creation date: Fri Oct 18 15:52:10 2024.
module Magix.BuilderSpec
  ( spec,
  )
where

import Magix (MagixOptions (..))
import Magix.Builder (buildArgs)
import Magix.Magix (Magix (..))
import Test.Hspec (Spec, describe, it, shouldBe, shouldContain)

magixOptions :: MagixOptions
magixOptions = MagixOptions "fakeScriptPath"

magix :: Magix
magix = HMagix ["fake", "packages"] ["fake", "flags"]

spec :: Spec
spec = do
  describe "buildArgs" $ do
    it "works correctly for some sample data" $ do
      let args = buildArgs magixOptions magix
      length args `shouldBe` 5
      args `shouldContain` ["--build-command"]
      args `shouldContain` ["--build-input"]
