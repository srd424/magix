-- |
-- Module      :  Magix.Directives.Common
-- Description :  Common tools for parsing directives
-- Copyright   :  2024 Dominik Schrempf
-- License     :  GPL-3.0-or-later
--
-- Maintainer  :  dominik.schrempf@gmail.com
-- Stability   :  experimental
-- Portability :  portable
--
-- Creation date: Fri Oct 18 09:17:40 2024.
module Magix.Directives.Common
  ( Parser,
    pDirectiveWithValue,
    pDirectiveWithValues,
    pMagixDirective,
  )
where

import Control.Applicative (Alternative (..))
import Data.Functor (($>))
import Data.Text (Text, pack)
import Data.Void (Void)
import Text.Megaparsec
  ( Parsec,
    chunk,
    sepBy1,
  )
import Text.Megaparsec.Char
  ( alphaNumChar,
    hspace,
    punctuationChar,
    symbolChar,
  )

type Parser = Parsec Void Text

pDirective :: Text -> Parser ()
pDirective d = chunk "#!" *> chunk d $> ()

pValue :: Parser Text
pValue = pack <$> some (alphaNumChar <|> punctuationChar <|> symbolChar)

pDirectiveWithValue :: Text -> Parser a -> Parser a
pDirectiveWithValue d p = pDirective d *> hspace *> p

pDirectiveWithValues :: Text -> Parser [Text]
pDirectiveWithValues d = pDirectiveWithValue d (sepBy1 pValue hspace)

pMagixDirective :: Text -> Parser ()
pMagixDirective x = pDirectiveWithValue "magix" (chunk x) $> ()
