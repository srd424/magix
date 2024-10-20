-- |
-- Module      :  Magix.Haskell.Directives
-- Description :  Haskell directives for Magix
-- Copyright   :  2024 Dominik Schrempf
-- License     :  GPL-3.0-or-later
--
-- Maintainer  :  dominik.schrempf@gmail.com
-- Stability   :  experimental
-- Portability :  portable
--
-- Creation date: Fri Oct 18 09:17:40 2024.
module Magix.Haskell.Directives
  ( HaskellDirectives (..),
    pHaskellDirectives,
  )
where

import Control.Applicative (Alternative (..))
import Data.Foldable (Foldable (..))
import Data.Text (Text)
import Magix.Directives.Common (Parser, pDirectiveWithValues, pMagixDirective)
import Text.Megaparsec (MonadParsec (notFollowedBy), chunk, sepEndBy, try)
import Text.Megaparsec.Char (newline, space1)
import Prelude hiding (readFile)

data HaskellDirectives = HaskellDirectives
  { _haskellPackages :: ![Text],
    _haskellGhcFlags :: ![Text]
  }
  deriving (Eq, Show)

data HaskellDirective
  = HaskellPackages ![Text]
  | HaskellGhcFlags ![Text]

pHaskellPackages :: Parser HaskellDirective
pHaskellPackages = HaskellPackages <$> try (pDirectiveWithValues "haskellPackages")

pHaskellGhcFlags :: Parser HaskellDirective
pHaskellGhcFlags = HaskellGhcFlags <$> try (pDirectiveWithValues "haskellGhcFlags")

pHaskellDirective :: Parser HaskellDirective
pHaskellDirective = pHaskellPackages <|> pHaskellGhcFlags

addHaskellDirective :: HaskellDirectives -> HaskellDirective -> HaskellDirectives
addHaskellDirective (HaskellDirectives ps fs) (HaskellPackages ps') =
  HaskellDirectives (ps <> ps') fs
addHaskellDirective (HaskellDirectives ps fs) (HaskellGhcFlags fs') =
  HaskellDirectives ps (fs <> fs')

pHaskellDirectives :: Parser HaskellDirectives
pHaskellDirectives = do
  pMagixDirective "haskell"
  space1
  ds <- sepEndBy pHaskellDirective newline
  notFollowedBy $ chunk "#!"
  let magix = foldl' addHaskellDirective (HaskellDirectives [] []) ds
  pure magix
