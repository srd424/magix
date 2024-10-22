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
import Magix.Directives.Common (Parser, pDirectiveWithValues, pLanguageDirectives)
import Prelude hiding (readFile)

newtype BashDirectives = BashDirectives {_runtimeInputs :: [Text]}
  deriving (Eq, Show, Semigroup, Monoid)

pRuntimeInputs :: Parser BashDirectives
pRuntimeInputs = BashDirectives <$> pDirectiveWithValues "runtimeInputs"

combineRuntimeInputs :: [BashDirectives] -> BashDirectives
combineRuntimeInputs = foldl' (<>) mempty

pBashDirectives :: Parser BashDirectives
pBashDirectives = pLanguageDirectives "bash" pRuntimeInputs combineRuntimeInputs
