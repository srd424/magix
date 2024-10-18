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
  ( LogLevel (..),
    MagixOptions (..),
    getMagixOptions,
  )
where

import Options.Applicative
  ( Parser,
    ParserInfo,
    auto,
    execParser,
    fullDesc,
    help,
    helper,
    info,
    long,
    metavar,
    option,
    progDesc,
    short,
    strArgument,
    value,
  )

data LogLevel = Info | Debug
  deriving (Eq, Read, Show)

data MagixOptions = MagixOptions
  { verbosity :: !LogLevel,
    scriptPath :: !FilePath
  }
  deriving (Eq, Show)

pMagixOptions :: Parser MagixOptions
pMagixOptions = MagixOptions <$> pLogLevel <*> pScriptPath

pScriptPath :: Parser FilePath
pScriptPath =
  strArgument
    (metavar "SCRIPT_FILE_PATH" <> help "File path of script to run")

pLogLevel :: Parser LogLevel
pLogLevel =
  option
    auto
    ( long "verbosity"
        <> short 'v'
        <> value Info
        <> metavar "LOG_LEVEL"
        <> help "Log level (Info or Debug)"
    )

desc :: String
desc = "Run and cache compiled scripts using the Nix package manager"

magixOptionsParser :: ParserInfo MagixOptions
magixOptionsParser = info (helper <*> pMagixOptions) (fullDesc <> progDesc desc)

getMagixOptions :: IO MagixOptions
getMagixOptions = execParser magixOptionsParser
