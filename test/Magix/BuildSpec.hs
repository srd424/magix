-- |
-- Module      :  Magix.BuildSpec
-- Description :  Unit tests of builder
-- Copyright   :  2024 Dominik Schrempf
-- License     :  GPL-3.0-or-later
--
-- Maintainer  :  dominik.schrempf@gmail.com
-- Stability   :  experimental
-- Portability :  portable
--
-- Creation date: Thu Oct 24 16:57:25 2024.
module Magix.BuildSpec
  ( spec,
  )
where

import Magix.Build (BuildStatus (..), getBuildStatus)
import Magix.Config (Config (..))
import Magix.Tools (getFakeConfigWithHash)
import System.Directory (removeFile)
import System.Posix (createSymbolicLink)
import System.Random (Random (..), newStdGen)
import Test.Hspec (Spec, describe, it, shouldBe)

getRandomHash :: IO Int
getRandomHash = fst . random <$> newStdGen

spec :: Spec
spec = do
  describe "getBuildStatus" $ do
    it "detects non-existing builds" $ do
      ha <- getRandomHash
      bs <- getBuildStatus (getFakeConfigWithHash ha)
      bs `shouldBe` NeedToBuild

    it "detects dangling symbolic links" $ do
      ha <- getRandomHash
      let c = getFakeConfigWithHash ha
          rp = c.resultLinkPath
      createSymbolicLink "nonexistent" rp
      bs <- getBuildStatus c
      bs `shouldBe` NeedToBuild
      removeFile rp
