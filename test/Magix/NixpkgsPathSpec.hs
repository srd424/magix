-- |
-- Module      :  Magix.NixpkgsPathSpec
-- Description :  Unit tests for
-- Copyright   :  2024 Dominik Schrempf
-- License     :  GPL-3.0-or-later
--
-- Maintainer  :  dominik.schrempf@gmail.com
-- Stability   :  experimental
-- Portability :  portable
--
-- Creation date: Thu Oct 24 15:46:40 2024.
module Magix.NixpkgsPathSpec
  ( spec,
  )
where

import Magix.NixpkgsPath (getDefaultNixpkgsPath, pNixPath, pNixpkgsPath)
import System.Environment (setEnv)
import Test.Hspec (Spec, describe, it, shouldBe)
import Text.Megaparsec (parse)

spec :: Spec
spec = do
  describe "pNixpkgsPath" $ do
    it "parses Nixpkgs path correctly" $ do
      parse pNixpkgsPath "" "nixpkgs=/path/to/nixpkgs"
        `shouldBe` Right "/path/to/nixpkgs"
      parse pNixpkgsPath "" "nixpkgs=/path/to/nixpkgs:some=/other/path"
        `shouldBe` Right "/path/to/nixpkgs"
      parse pNixpkgsPath "" "nixpkgs=/path/to/nixpkgs some=/other/path"
        `shouldBe` Right "/path/to/nixpkgs"

  describe "pNixPath" $ do
    it "parses NIX_PATH correctly" $ do
      parse pNixPath "" "nixpkgs=/path/to/nixpkgs"
        `shouldBe` Right "/path/to/nixpkgs"
      parse pNixPath "" "nixpkgs=/path/to/nixpkgs:some=/other/path"
        `shouldBe` Right "/path/to/nixpkgs"
      parse pNixPath "" "other=/path/here:nixpkgs=/path/to/nixpkgs:some=/other/path"
        `shouldBe` Right "/path/to/nixpkgs"
      parse pNixPath "" "other=/path/here:and=/another:nixpkgs=/path/to/nixpkgs"
        `shouldBe` Right "/path/to/nixpkgs"
      parse pNixPath "" "other=/path/here:and=/another:nixpkgs=/path/to/nixpkgs some=/other/path"
        `shouldBe` Right "/path/to/nixpkgs"
      parse pNixPath "" "nixpkgs=/nix/store/lsy6c2f9alj2gkjj36h754kk63x6701l-source"
        `shouldBe` Right "/nix/store/lsy6c2f9alj2gkjj36h754kk63x6701l-source"

  describe "pDefaultNixpkgsPath" $ do
    it "works for a sample value" $ do
      setEnv "NIX_PATH" "nixpkgs=/path/to/nixpkgs"
      nixpkgsPath <- getDefaultNixpkgsPath
      nixpkgsPath `shouldBe` Right "/path/to/nixpkgs"
