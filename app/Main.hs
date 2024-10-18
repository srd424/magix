-- |
-- Module      :  Main
-- Description :  Magically run and cache compiled scripts
-- Copyright   :  2024 Dominik Schrempf
-- License     :  GPL-3.0-or-later
--
-- Maintainer  :  dominik.schrempf@gmail.com
-- Stability   :  experimental
-- Portability :  portable
--
-- Creation date: Fri Oct 18 09:12:06 2024.
module Main
  ( main,
  )
where

import Data.Text.IO (readFile)
import Magix.Directives (pMagix)
import Options (MagixOptions (scriptFilePath), magixOptionsParser)
import Options.Applicative (execParser)
import System.OsPath (decodeUtf)
import Text.Megaparsec (errorBundlePretty, parse)
import Prelude hiding (readFile)

main :: IO ()
main = do
  opts <- execParser magixOptionsParser
  print opts
  let script = scriptFilePath opts
  path <- decodeUtf script
  contents <- readFile path
  let magix =
        either (error . errorBundlePretty) id $
          parse pMagix path contents
  print magix
