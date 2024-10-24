-- |
-- Module      :  Magix.NixpkgsPath
-- Description :  Get path to Nixpkgs
-- Copyright   :  2024 Dominik Schrempf
-- License     :  GPL-3.0-or-later
--
-- Maintainer  :  dominik.schrempf@gmail.com
-- Stability   :  experimental
-- Portability :  portable
--
-- Creation date: Thu Oct 24 06:47:24 2024.
module Magix.NixpkgsPath
  ( pNixpkgsPath,
    pNixPath,
    getDefaultNixpkgsPath,
  )
where

import Data.Bifunctor (Bifunctor (..))
import Data.Char (isSpace)
import Data.Void (Void)
import System.Environment (getEnv)
import Text.Megaparsec (MonadParsec (takeWhile1P), Parsec, anySingle, chunk, errorBundlePretty, parse, (<|>))

type Parser = Parsec Void String

isValidPathChar :: Char -> Bool
isValidPathChar x = x /= ':' && not (isSpace x)

pNixpkgsPath :: Parser FilePath
pNixpkgsPath =
  chunk "nixpkgs="
    *> takeWhile1P
      (Just "pathComponents")
      isValidPathChar

pNixPath :: Parser FilePath
pNixPath = pNixpkgsPath <|> (anySingle *> pNixPath)

getDefaultNixpkgsPath :: IO (Either String FilePath)
getDefaultNixpkgsPath = do
  nixPath <- getEnv "NIX_PATH"
  pure $ first errorBundlePretty $ parse pNixPath "" nixPath
