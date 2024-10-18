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
import Directive (pShebang)
import Test.Hspec (Spec, describe, it, shouldBe)
import Text.Megaparsec (parse)

magixShebang :: Text
magixShebang = "#!/usr/bin/env magix"

spec :: Spec
spec = do
  describe "pShebang" $ do
    it "parses the shebang" $
      parse pShebang "" magixShebang `shouldBe` Right ()
