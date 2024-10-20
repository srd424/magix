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
  ( Directives (..),
    pShebang,
    pDirectives,
    getDirectives,
  )
where

import Data.Text (Text)
import Magix.Directives.Common (Parser)
import Magix.Haskell.Directives (HaskellDirectives (..), pHaskellDirectives)
import Text.Megaparsec (chunk, errorBundlePretty, parse)
import Text.Megaparsec.Char (space1)
import Prelude hiding (readFile)

newtype Directives = Haskell HaskellDirectives
  deriving (Eq, Show)

pShebang :: Parser Text
pShebang = chunk "#!/usr/bin/env magix"

pDirectives :: Parser Directives
pDirectives = pShebang *> space1 *> (Haskell <$> pHaskellDirectives)

getDirectives :: FilePath -> Text -> Directives
getDirectives p x = either (error . errorBundlePretty) id $ parse pDirectives p x
