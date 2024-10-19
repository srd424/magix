-- |
-- Module      :  Magix.Directives.CommonSpec
-- Description :  Tests for parsing common directives
-- Copyright   :  2024 Dominik Schrempf
-- License     :  GPL-3.0-or-later
--
-- Maintainer  :  dominik.schrempf@gmail.com
-- Stability   :  experimental
-- Portability :  portable
--
-- Creation date: Fri Oct 18 09:34:01 2024.
module Magix.Directives.CommonSpec
  ( spec,
  )
where

import Data.Text (Text)
import Magix.Directives.Common (pMagixDirective)
import Test.Hspec (Spec, describe, it, shouldBe)
import Text.Megaparsec (parse)
import Prelude hiding (readFile)

haskellMagixDirective :: Text
haskellMagixDirective = "#!magix haskell"

spec :: Spec
spec = do
  describe "pMagixDirective" $ do
    it "parses a sample Magix directive" $
      parse (pMagixDirective "haskell") "" haskellMagixDirective `shouldBe` Right ()
