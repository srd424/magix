-- |
-- Module      :  Magix.Options
-- Description :  Magical options
-- Copyright   :  2024 Dominik Schrempf
-- License     :  GPL-3.0-or-later
--
-- Maintainer  :  dominik.schrempf@gmail.com
-- Stability   :  experimental
-- Portability :  portable
--
-- Creation date: Fri Oct 18 10:37:48 2024.
module Magix.Options
  ( MagixOptions (..),
    getMagixOptions,
  )
where

import Options.Applicative
  ( Parser,
    ParserInfo,
    execParser,
    fullDesc,
    help,
    helper,
    info,
    metavar,
    progDesc,
    strArgument,
  )

newtype MagixOptions = MagixOptions {scriptFilePath :: FilePath}
  deriving (Eq, Show)

pMagixOptions :: Parser MagixOptions
pMagixOptions = MagixOptions <$> pScriptFilePath

pScriptFilePath :: Parser FilePath
pScriptFilePath =
  strArgument
    (metavar "SCRIPT_FILE_PATH" <> help "File path of script to run")

desc :: String
desc = "Run and cache compiled scripts using the Nix package manager"

magixOptionsParser :: ParserInfo MagixOptions
magixOptionsParser = info (helper <*> pMagixOptions) (fullDesc <> progDesc desc)

getMagixOptions :: IO MagixOptions
getMagixOptions = execParser magixOptionsParser
