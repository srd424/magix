-- |
-- Module      :  Option
-- Description :  Magical options
-- Copyright   :  2024 Dominik Schrempf
-- License     :  GPL-3.0-or-later
--
-- Maintainer  :  dominik.schrempf@gmail.com
-- Stability   :  experimental
-- Portability :  portable
--
-- Creation date: Fri Oct 18 10:37:48 2024.
module Option
  ( MagixOptions (..),
    magixOptionsParser,
  )
where

import Options.Applicative
  ( Parser,
    ParserInfo,
    argument,
    eitherReader,
    fullDesc,
    help,
    helper,
    info,
    metavar,
    progDesc,
  )
import System.OsPath (OsPath, encodeUtf)

newtype MagixOptions = MagixOptions {scriptName :: OsPath}
  deriving (Eq, Show)

pMagixOptions :: Parser MagixOptions
pMagixOptions = MagixOptions <$> pScriptName

readPath :: String -> Either String OsPath
readPath x = case encodeUtf x of
  Left err -> Left $ show err
  Right p -> Right p

pScriptName :: Parser OsPath
pScriptName =
  argument
    (eitherReader readPath)
    (metavar "SCRIPT_FILE_PATH" <> help "File path of script to run")

desc :: String
desc = "Run and cache compiled scripts using the Nix package manager"

magixOptionsParser :: ParserInfo MagixOptions
magixOptionsParser = info (helper <*> pMagixOptions) (fullDesc <> progDesc desc)
