-- |
-- Module      :  Magix.Haskell.BuilderSpec
-- Description :  Unit tests for BuilderSpec
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

import Magix (MagixOptions (..))
import Magix.Haskell.Builder (buildHaskellArgs)
import Magix.Haskell.Directives (HaskellMagix (..))
import Magix.Options (LogLevel (..))
import Test.Hspec (Spec, describe, it, shouldBe, shouldContain)

magixOptions :: MagixOptions
magixOptions = MagixOptions Info "fakeScriptPath"

magix :: HaskellMagix
magix = HaskellMagix ["fake", "packages"] ["fake", "flags"]

spec :: Spec
spec = do
  describe "buildHaskellArgs" $ do
    it "works correctly for some sample data" $ do
      let args = buildHaskellArgs magixOptions magix
      length args `shouldBe` 5
      args `shouldContain` ["--build-command"]
      args `shouldContain` ["--build-input"]
