-- |
-- Module      :  Directive
-- Description :  Parse directives
-- Copyright   :  2024 Dominik Schrempf
-- License     :  GPL-3.0-or-later
--
-- Maintainer  :  dominik.schrempf@gmail.com
-- Stability   :  experimental
-- Portability :  portable
--
-- Creation date: Fri Oct 18 09:17:40 2024.
module Directive
  ( pDirectiveShebang,
    pDirectiveMagix,
    pMagix,
  )
where

import Data.Functor (($>))
import Data.Text (Text, pack)
import Data.Void (Void)
import Magix (Magix (..))
import Text.Megaparsec (Parsec, chunk, sepBy, some)
import Text.Megaparsec.Char (alphaNumChar, hspace, space1)

type Parser = Parsec Void Text

pDirective :: Text -> Parser ()
pDirective d = chunk "#!" *> chunk d $> ()

pValue :: Parser Text
pValue = pack <$> some alphaNumChar

pDirectiveWithValue :: Text -> Parser a -> Parser a
pDirectiveWithValue d p = pDirective d *> hspace *> p

pDirectiveWithValues :: Text -> Parser [Text]
pDirectiveWithValues d = pDirectiveWithValue d (sepBy pValue hspace)

pDirectiveShebang :: Parser ()
pDirectiveShebang = chunk "#!/usr/bin/env magix" $> ()

pMagix :: Parser Magix
pMagix = pDirectiveShebang *> space1 *> pHMagix

pDirectiveMagix :: Text -> Parser ()
pDirectiveMagix x = pDirectiveWithValue "magix" (chunk x) $> ()

pHMagix :: Parser Magix
pHMagix = do
  pDirectiveMagix "haskell"
  space1
  ps <- pDirectiveWithValues "haskellPackages"
  pure $ HMagix ps
