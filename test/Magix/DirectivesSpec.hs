-- |
-- Module      :  Magix.DirectivesSpec
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

import Data.Either (isRight)
import Data.Text (Text)
import Data.Text.IO (readFile)
import Magix.Directives (Directives (..), pDirectives, pShebang)
import Magix.Languages.Haskell.Directives (HaskellDirectives (..))
import Test.Hspec (Spec, describe, it, shouldBe, shouldSatisfy)
import Text.Megaparsec (parse)
import Prelude hiding (readFile)

magixShebang :: Text
magixShebang = "#!/usr/bin/env magix"

fnMinimal :: FilePath
fnMinimal = "test-scripts/minimal-haskell"

readMinimal :: IO Text
readMinimal = readFile fnMinimal

spec :: Spec
spec = do
  describe "pShebang" $ do
    it "parses the shebang" $
      parse pShebang "" magixShebang `shouldSatisfy` isRight

  describe "pDirectives" $ do
    it "parses a minimal sample script" $ do
      minimal <- readMinimal
      parse pDirectives fnMinimal minimal
        `shouldBe` Right (Haskell (HaskellDirectives ["bytestring"] ["-threaded"]))
