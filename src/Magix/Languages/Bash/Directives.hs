-- |
-- Module      :  Magix.Languages.Bash.Directives
-- Description :  Bash directives for Magix
-- Copyright   :  2024 Dominik Schrempf
-- License     :  GPL-3.0-or-later
--
-- Maintainer  :  dominik.schrempf@gmail.com
-- Stability   :  experimental
-- Portability :  portable
--
-- Creation date: Fri Oct 18 09:17:40 2024.
module Magix.Languages.Bash.Directives
  ( BashDirectives (..),
    pBashDirectives,
  )
where

import Data.Foldable (Foldable (..))
import Data.Text (Text)
import Magix.Directives.Common (Parser, pDirectiveWithValues, pMagixDirective)
import Text.Megaparsec (MonadParsec (notFollowedBy), chunk, sepEndBy, try)
import Text.Megaparsec.Char (newline, space1)
import Prelude hiding (readFile)

newtype BashDirectives = BashDirectives {_runtimeInputs :: [Text]} deriving (Eq, Show)

pRuntimeInputs :: Parser BashDirectives
pRuntimeInputs = BashDirectives <$> try (pDirectiveWithValues "runtimeInputs")

addBashDirective :: BashDirectives -> BashDirectives -> BashDirectives
addBashDirective (BashDirectives ps) (BashDirectives ps') =
  BashDirectives (ps <> ps')

pBashDirectives :: Parser BashDirectives
pBashDirectives = do
  pMagixDirective "bash"
  space1
  ds <- sepEndBy pRuntimeInputs newline
  notFollowedBy $ chunk "#!"
  let magix = foldl' addBashDirective (BashDirectives []) ds
  pure magix
