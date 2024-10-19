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

data HaskellMagixDirective
  = HaskellMagixHaskellPackages ![Text]
  | HaskellMagixGhcFlags ![Text]

pHaskellPackages :: Parser HaskellMagixDirective
pHaskellPackages = HaskellMagixHaskellPackages <$> try (pDirectiveWithValues "haskellPackages")

pHaskellGhcFlags :: Parser HaskellMagixDirective
pHaskellGhcFlags = HaskellMagixGhcFlags <$> try (pDirectiveWithValues "haskellGhcFlags")

pHaskellMagixDirective :: Parser HaskellMagixDirective
pHaskellMagixDirective = pHaskellPackages <|> pHaskellGhcFlags

addHaskellMagixDirective :: HaskellMagix -> HaskellMagixDirective -> HaskellMagix
addHaskellMagixDirective (HaskellMagix ps fs) (HaskellMagixHaskellPackages ps') =
  HaskellMagix (ps <> ps') fs
addHaskellMagixDirective (HaskellMagix ps fs) (HaskellMagixGhcFlags fs') =
  HaskellMagix ps (fs <> fs')

pHaskellMagix :: Parser HaskellMagix
pHaskellMagix = do
  pMagixDirective "haskell"
  space1
  ds <- sepEndBy pHaskellMagixDirective newline
  notFollowedBy $ chunk "#!"
  let magix = foldl' addHaskellMagixDirective (HaskellMagix [] []) ds
  pure magix
