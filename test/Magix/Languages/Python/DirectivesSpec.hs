-- |
-- Module      :  Magix.Languages.Python.DirectivesSpec
-- Description :  Tests for parsing Python directives
-- Copyright   :  2024 Dominik Schrempf
-- License     :  GPL-3.0-or-later
--
-- Maintainer  :  dominik.schrempf@gmail.com
-- Stability   :  experimental
-- Portability :  portable
--
-- Creation date: Fri Oct 18 09:34:01 2024.
module Magix.Languages.Python.DirectivesSpec
  ( spec,
  )
where

import Data.Text (Text, unlines)
import Magix.Languages.Python.Directives (PythonDirectives (..), pPythonDirectives)
import Test.Hspec (Spec, describe, it, shouldBe)
import Text.Megaparsec (parse)
import Prelude hiding (unlines)

empty :: Text
empty = "#!magix python"

minimal :: Text
minimal =
  unlines
    [ "#!magix python",
      "#!pythonPackages numpy"
    ]

multiple :: Text
multiple =
  unlines
    [ "#!magix python",
      "#!pythonPackages a",
      "#!pythonPackages b c",
      "#!pythonPackages d e f"
    ]

spec :: Spec
spec = do
  describe "pPythonDirectives" $ do
    it "parses empty directives" $ do
      parse pPythonDirectives "" empty
        `shouldBe` Right (PythonDirectives [])

    it "parses minimal sample directives" $ do
      parse pPythonDirectives "" minimal
        `shouldBe` Right (PythonDirectives ["numpy"])

    it "parses more interesting sample directives with multiple declarations" $ do
      parse pPythonDirectives "" multiple
        `shouldBe` Right (PythonDirectives ["a", "b", "c", "d", "e", "f"])
