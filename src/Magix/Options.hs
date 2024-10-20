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
    Options (..),
    getOptions,
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

data Options = Options
  { verbosity :: !Verbosity,
    scriptPath :: !FilePath
  }
  deriving (Eq, Show)

pOptions :: Parser Options
pOptions = Options <$> pLogLevel <*> pScriptPath

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

optionsParser :: ParserInfo Options
optionsParser = info (helper <*> pOptions) (fullDesc <> progDesc desc)

getOptions :: IO Options
getOptions = execParser optionsParser
