-- |
-- Module      :  Magix.Haskell.DirectivesSpec
-- Description :  Tests for parsing Haskell directives
-- Copyright   :  2024 Dominik Schrempf
-- License     :  GPL-3.0-or-later
--
-- Maintainer  :  dominik.schrempf@gmail.com
-- Stability   :  experimental
-- Portability :  portable
--
-- Creation date: Fri Oct 18 09:34:01 2024.
module Magix.Haskell.DirectivesSpec
  ( spec,
  )
where

import Data.Text (Text, unlines)
import Magix.Haskell.Directives (HaskellMagix (..), pHaskellMagix)
import Test.Hspec (Spec, describe, it, shouldBe)
import Text.Megaparsec (parse)
import Prelude hiding (readFile, unlines)

minimal :: Text
minimal =
  unlines
    [ "#!magix haskell",
      "#!haskellPackages bytestring",
      "#!haskellGhcFlags -threaded"
    ]

multiple :: Text
multiple =
  unlines
    [ "#!magix haskell",
      "#!haskellPackages a",
      "#!haskellPackages b c",
      "#!haskellGhcFlags 1",
      "#!haskellPackages d e f",
      "#!haskellGhcFlags 2",
      "#!haskellGhcFlags 3 4"
    ]

spec :: Spec
spec = do
  describe "pHaskellMagix" $ do
    it "parses minimal sample directives" $ do
      parse pHaskellMagix "" minimal
        `shouldBe` Right (HaskellMagix ["bytestring"] ["-threaded"])

  describe "pHaskellMagix" $ do
    it "parses more interesting sample directives with multiple declarations" $ do
      parse pHaskellMagix "" multiple
        `shouldBe` Right (HaskellMagix ["a", "b", "c", "d", "e", "f"] ["1", "2", "3", "4"])
