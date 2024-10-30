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
    getLanguageName,
    pShebang,
    pDirectives,
    getDirectives,
  )
where

import Control.Applicative ((<|>))
import Data.Bifunctor (Bifunctor (..))
import Data.Text (Text)
import Magix.Directives.Common (Parser)
import Magix.Languages.Bash.Directives (BashDirectives (..), pBashDirectives)
import Magix.Languages.Haskell.Directives (HaskellDirectives (..), pHaskellDirectives)
import Magix.Languages.Python.Directives (PythonDirectives, pPythonDirectives)
import Text.Megaparsec (MonadParsec (..), chunk, errorBundlePretty, parse)
import Text.Megaparsec.Char (space1)
import Prelude hiding (readFile)

data Directives
  = Bash !BashDirectives
  | Haskell !HaskellDirectives
  | Python !PythonDirectives
  deriving (Eq, Show)

-- | Use the language name to find the Nix expression template.
getLanguageName :: Directives -> String
getLanguageName (Bash _) = "Bash"
getLanguageName (Haskell _) = "Haskell"
getLanguageName (Python _) = "Python"

pShebang :: Parser Text
pShebang = chunk "#!/usr/bin/env magix"

pLanguageSpecificDirectives :: Parser Directives
pLanguageSpecificDirectives =
  (Bash <$> try pBashDirectives)
    <|> (Haskell <$> try pHaskellDirectives)
    <|> (Python <$> pPythonDirectives)

pDirectives :: Parser Directives
pDirectives = pShebang *> space1 *> pLanguageSpecificDirectives

getDirectives :: FilePath -> Text -> Either String Directives
getDirectives p x = first errorBundlePretty $ parse pDirectives p x
