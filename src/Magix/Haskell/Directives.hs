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
  ( HaskellMagix (..),
    pHaskellMagix,
  )
where

import Control.Applicative (Alternative (..))
import Data.Foldable (Foldable (..))
import Data.Text (Text)
import Magix.Directives.Common (Parser, pDirectiveWithValues, pMagixDirective)
import Text.Megaparsec (MonadParsec (notFollowedBy), chunk, sepEndBy, try)
import Text.Megaparsec.Char (newline, space1)
import Prelude hiding (readFile)

data HaskellMagix = HaskellMagix
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

addHaskellDirective :: HaskellMagix -> HaskellDirective -> HaskellMagix
addHaskellDirective (HaskellMagix ps fs) (HaskellPackages ps') =
  HaskellMagix (ps <> ps') fs
addHaskellDirective (HaskellMagix ps fs) (HaskellGhcFlags fs') =
  HaskellMagix ps (fs <> fs')

pHaskellMagix :: Parser HaskellMagix
pHaskellMagix = do
  pMagixDirective "haskell"
  space1
  ds <- sepEndBy pHaskellDirective newline
  notFollowedBy $ chunk "#!"
  let magix = foldl' addHaskellDirective (HaskellMagix [] []) ds
  pure magix
