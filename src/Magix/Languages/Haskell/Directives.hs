{-# LANGUAGE LambdaCase #-}

-- |
-- Module      :  Magix.Languages.Haskell.Directives
-- Description :  Haskell directives for Magix
-- Copyright   :  2024 Dominik Schrempf
-- License     :  GPL-3.0-or-later
--
-- Maintainer  :  dominik.schrempf@gmail.com
-- Stability   :  experimental
-- Portability :  portable
--
-- Creation date: Fri Oct 18 09:17:40 2024.
module Magix.Languages.Haskell.Directives
  ( HaskellDirectives (..),
    pHaskellDirectives,
  )
where

import Control.Applicative (Alternative (..))
import Data.Foldable (Foldable (..))
import Data.Text (Text)
import Magix.Directives.Common (Parser, pDirectiveWithValues, pLanguageDirectives)
import Text.Megaparsec (try)
import Prelude hiding (readFile)

data HaskellDirectives = HaskellDirectives
  { _haskellPackages :: ![Text],
    _ghcFlags :: ![Text]
  }
  deriving (Eq, Show)

data HaskellDirective = HaskellPackages ![Text] | GhcFlags ![Text]

pHaskellPackages :: Parser HaskellDirective
pHaskellPackages = HaskellPackages <$> pDirectiveWithValues "haskellPackages"

pGhcFlags :: Parser HaskellDirective
pGhcFlags = GhcFlags <$> pDirectiveWithValues "ghcFlags"

pHaskellDirective :: Parser HaskellDirective
pHaskellDirective = try pHaskellPackages <|> pGhcFlags

addHaskellDirective :: HaskellDirectives -> HaskellDirective -> HaskellDirectives
addHaskellDirective (HaskellDirectives ps fs) = \case
  (HaskellPackages ps') -> HaskellDirectives (ps <> ps') fs
  (GhcFlags fs') -> HaskellDirectives ps (fs <> fs')

combineHaskellDirectives :: [HaskellDirective] -> HaskellDirectives
combineHaskellDirectives = foldl' addHaskellDirective (HaskellDirectives [] [])

pHaskellDirectives :: Parser HaskellDirectives
pHaskellDirectives = pLanguageDirectives "haskell" pHaskellDirective combineHaskellDirectives
