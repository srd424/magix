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
  (
  )
where

import Data.Char (isSpace)
import Data.Void (Void)
import System.Environment (getEnv)
import Text.Megaparsec (MonadParsec (takeWhile1P), Parsec, chunk)

getDefaultNixpkgsPath :: IO FilePath
getDefaultNixpkgsPath = do
  nixPath <- getEnv "NIX_PATH"
  pure $ nixPath

type Parser = Parsec Void String

pNixpkgsPath :: Parser FilePath
pNixpkgsPath =
  chunk "nixpkgs="
    *> takeWhile1P
      (Just "pathComponents")
      (\x -> x == ':' || not (isSpace x))

pNixPath :: Parser FilePath
pNixPath = undefined
