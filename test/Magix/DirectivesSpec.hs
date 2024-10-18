-- |
-- Module      :  DirectivesSpec
-- Description :  Unit tests for Directive
-- Copyright   :  2024 Dominik Schrempf
-- License     :  GPL-3.0-or-later
--
-- Maintainer  :  dominik.schrempf@gmail.com
-- Stability   :  experimental
-- Portability :  portable
--
-- Creation date: Fri Oct 18 09:34:01 2024.
module Magix.DirectivesSpec
  ( spec,
  )
where

import Data.Text (Text)
import Data.Text.IO (readFile)
import Magix.Directives (pDirectiveMagix, pDirectiveShebang, pMagix)
import Magix.Magix (Magix (..))
import Test.Hspec (Spec, describe, it, shouldBe)
import Text.Megaparsec (parse)
import Prelude hiding (readFile)

magixShebang :: Text
magixShebang = "#!/usr/bin/env magix"

hMagixDirective :: Text
hMagixDirective = "#!magix haskell"

fnMinimal :: FilePath
fnMinimal = "test-scripts/minimal"

readMinimal :: IO Text
readMinimal = readFile fnMinimal

fnMultiple :: FilePath
fnMultiple = "test-scripts/multiple"

readMultiple :: IO Text
readMultiple = readFile fnMultiple

spec :: Spec
spec = do
  describe "pDirectiveShebang" $ do
    it "parses the shebang" $
      parse pDirectiveShebang "" magixShebang `shouldBe` Right ()

  describe "pDirectiveMagix" $ do
    it "parses a sample Magix directive" $
      parse (pDirectiveMagix "haskell") "" hMagixDirective `shouldBe` Right ()

  describe "pMagix" $ do
    it "parses a minimal sample script" $ do
      minimal <- readMinimal
      parse pMagix fnMinimal minimal `shouldBe` Right (HMagix ["bytestring"] ["-threaded"])

  describe "pMagix" $ do
    it "parses a more interesting sample script with multiple directives" $ do
      multiple <- readMultiple
      parse pMagix fnMultiple multiple
        `shouldBe` Right (HMagix ["a", "b", "c", "d", "e", "f"] ["1", "2", "3", "4"])
