-- |
-- Module      :  Directives
-- Description :  Parse directives
-- Copyright   :  2024 Dominik Schrempf
-- License     :  GPL-3.0-or-later
--
-- Maintainer  :  dominik.schrempf@gmail.com
-- Stability   :  experimental
-- Portability :  portable
--
-- Creation date: Fri Oct 18 09:17:40 2024.
module Magix.Directives
  ( pDirectiveShebang,
    pDirectiveMagix,
    pMagix,
    getDirectives,
  )
where

import Control.Applicative (Alternative (..))
import Data.Foldable (Foldable (..))
import Data.Functor (($>))
import Data.Text (Text, pack)
import Data.Text.IO (readFile)
import Data.Void (Void)
import Magix.Magix (Magix (..))
import Text.Megaparsec
  ( MonadParsec (notFollowedBy),
    Parsec,
    chunk,
    errorBundlePretty,
    parse,
    sepBy1,
    sepEndBy,
    try,
  )
import Text.Megaparsec.Char
  ( alphaNumChar,
    hspace,
    newline,
    punctuationChar,
    space1,
    symbolChar,
  )
import Prelude hiding (readFile)

type Parser = Parsec Void Text

pDirective :: Text -> Parser ()
pDirective d = chunk "#!" *> chunk d $> ()

pValue :: Parser Text
pValue = pack <$> some (alphaNumChar <|> punctuationChar <|> symbolChar)

pDirectiveWithValue :: Text -> Parser a -> Parser a
pDirectiveWithValue d p = pDirective d *> hspace *> p

pDirectiveWithValues :: Text -> Parser [Text]
pDirectiveWithValues d = pDirectiveWithValue d (sepBy1 pValue hspace)

pDirectiveShebang :: Parser ()
pDirectiveShebang = chunk "#!/usr/bin/env magix" $> ()

pDirectiveMagix :: Text -> Parser ()
pDirectiveMagix x = pDirectiveWithValue "magix" (chunk x) $> ()

data HMagixDirective = HMagixHaskellPackages ![Text] | HMagixGhcFlags ![Text]

pHaskellPackages :: Parser HMagixDirective
pHaskellPackages = HMagixHaskellPackages <$> try (pDirectiveWithValues "haskellPackages")

pHaskellGhcFlags :: Parser HMagixDirective
pHaskellGhcFlags = HMagixGhcFlags <$> try (pDirectiveWithValues "haskellGhcFlags")

pHMagixDirective :: Parser HMagixDirective
pHMagixDirective = pHaskellPackages <|> pHaskellGhcFlags

addHMagixDirective :: Magix -> HMagixDirective -> Magix
addHMagixDirective (HMagix ps fs) (HMagixHaskellPackages ps') = HMagix (ps <> ps') fs
addHMagixDirective (HMagix ps fs) (HMagixGhcFlags fs') = HMagix ps (fs <> fs')

pHMagix :: Parser Magix
pHMagix = do
  pDirectiveMagix "haskell"
  space1
  ds <- sepEndBy pHMagixDirective newline
  notFollowedBy $ chunk "#!"
  let magix = foldl' addHMagixDirective (HMagix [] []) ds
  pure magix

pMagix :: Parser Magix
pMagix = pDirectiveShebang *> space1 *> pHMagix

getDirectives :: FilePath -> IO Magix
getDirectives x = do
  contents <- readFile x
  let magix = either (error . errorBundlePretty) id $ parse pMagix x contents
  pure magix
