-- |
-- Module      :  DirectiveSpec
-- Description :  Unit tests for Directive
-- Copyright   :  2024 Dominik Schrempf
-- License     :  GPL-3.0-or-later
--
-- Maintainer  :  dominik.schrempf@gmail.com
-- Stability   :  experimental
-- Portability :  portable
--
-- Creation date: Fri Oct 18 09:34:01 2024.
module DirectiveSpec
  ( spec,
  )
where

import Data.Text (Text)
import Data.Text.IO (readFile)
import Directive (pDirectiveMagix, pDirectiveShebang, pMagix)
import Magix (Magix (..))
import Test.Hspec (Spec, describe, it, shouldBe)
import Text.Megaparsec (parse)
import Prelude hiding (readFile)

magixShebang :: Text
magixShebang = "#!/usr/bin/env magix"

hMagixDirective :: Text
hMagixDirective = "#!magix haskell"

fnMinimal :: FilePath
fnMinimal = "test/scripts/minimal"

readMinimal :: IO Text
readMinimal = readFile fnMinimal

spec :: Spec
spec = do
  describe "pDirectiveShebang" $ do
    it "parses the shebang" $
      parse pDirectiveShebang "" magixShebang `shouldBe` Right ()

  describe "pDirectiveMagix" $ do
    it "parses a sample Magix directive" $
      parse (pDirectiveMagix "haskell") "" hMagixDirective `shouldBe` Right ()

  describe "pMagix" $ do
    it "parses s sample script" $ do
      minimal <- readMinimal
      parse pMagix fnMinimal minimal `shouldBe` Right (HMagix ["bytestring"])
