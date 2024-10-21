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
    Rebuild (..),
    Options (..),
    getOptions,
  )
where

import Control.Applicative (Alternative (..))
import Options.Applicative
  ( Parser,
    ParserInfo (infoPolicy),
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
import Options.Applicative.Types (ArgPolicy (..))

data Verbosity = Info | Debug deriving (Eq, Show)

data Rebuild = ReuseBuildIfAvailable | ForceBuild deriving (Eq, Show)

data Options = Options
  { verbosity :: !Verbosity,
    forceBuild :: !Rebuild,
    scriptPath :: !FilePath,
    scriptArgs :: ![String]
  }
  deriving (Eq, Show)

pOptions :: Parser Options
pOptions = Options <$> pLogLevel <*> pForceBuild <*> pScriptPath <*> pScriptArgs

pLogLevel :: Parser Verbosity
pLogLevel =
  flag
    Info
    Debug
    ( long "verbose"
        <> short 'v'
        <> help "Print debugging messages"
    )

pForceBuild :: Parser Rebuild
pForceBuild =
  flag
    ReuseBuildIfAvailable
    ForceBuild
    ( long "force-build"
        <> short 'f'
        <> help "Force build, even when cached build exists"
    )

pScriptPath :: Parser FilePath
pScriptPath =
  strArgument
    (metavar "SCRIPT_FILE_PATH" <> help "File path of script to build, cache and run")

pScriptArgs :: Parser [String]
pScriptArgs =
  many $
    strArgument
      ( metavar "SCRIPT_ARGS"
          <> help "Arguments passed on to the script"
      )

desc :: String
desc = "Run and cache compiled scripts using the Nix package manager"

optionsParser :: ParserInfo Options
optionsParser = info (helper <*> pOptions) (fullDesc <> progDesc desc)

getOptions :: IO Options
getOptions = execParser (optionsParser {infoPolicy = NoIntersperse})
