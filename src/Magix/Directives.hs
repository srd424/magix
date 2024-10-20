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

import Control.Applicative ((<|>))
import Control.Monad.Catch (MonadThrow (throwM))
import Data.Text (Text)
import Magix.Directives.Common (Parser)
import Magix.Languages.Bash.Directives (BashDirectives (..), pBashDirectives)
import Magix.Languages.Haskell.Directives (HaskellDirectives (..), pHaskellDirectives)
import Text.Megaparsec (MonadParsec (..), chunk, parse)
import Text.Megaparsec.Char (space1)
import Prelude hiding (readFile)

data Directives = Haskell !HaskellDirectives | Bash !BashDirectives
  deriving (Eq, Show)

pShebang :: Parser Text
pShebang = chunk "#!/usr/bin/env magix"

pLanguageSpecificDirectives :: Parser Directives
pLanguageSpecificDirectives =
  (Haskell <$> try pHaskellDirectives)
    <|> (Bash <$> pBashDirectives)

pDirectives :: Parser Directives
pDirectives = pShebang *> space1 *> pLanguageSpecificDirectives

getDirectives :: (MonadThrow m) => FilePath -> Text -> m Directives
getDirectives p x = either throwM pure $ parse pDirectives p x
