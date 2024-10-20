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
  ( Verbosity (..),
    MagixOptions (..),
    getMagixOptions,
  )
where

import Options.Applicative
  ( Parser,
    ParserInfo,
    execParser,
    flag,
    fullDesc,
    help,
    helper,
    info,
    long,
    metavar,
    progDesc,
    short,
    strArgument,
  )

data Verbosity = Info | Debug deriving (Eq, Show)

data MagixOptions = MagixOptions
  { verbosity :: !Verbosity,
    scriptPath :: !FilePath
  }
  deriving (Eq, Show)

pMagixOptions :: Parser MagixOptions
pMagixOptions = MagixOptions <$> pLogLevel <*> pScriptPath

pScriptPath :: Parser FilePath
pScriptPath =
  strArgument
    (metavar "SCRIPT_FILE_PATH" <> help "File path of script to run")

pLogLevel :: Parser Verbosity
pLogLevel =
  flag
    Info
    Debug
    ( long "verbose"
        <> short 'v'
        <> help "Print debugging messages"
    )

desc :: String
desc = "Run and cache compiled scripts using the Nix package manager"

magixOptionsParser :: ParserInfo MagixOptions
magixOptionsParser = info (helper <*> pMagixOptions) (fullDesc <> progDesc desc)

getMagixOptions :: IO MagixOptions
getMagixOptions = execParser magixOptionsParser
