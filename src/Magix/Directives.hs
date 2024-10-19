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
  ( pMagix,
    getDirectives,
  )
where

import Data.Text (Text)
import Data.Text.IO (readFile)
import Magix.Directives.Common (Parser)
import Magix.Haskell.Directives (pHaskellMagix)
import Magix.Magix (Magix (..))
import Text.Megaparsec (chunk, errorBundlePretty, parse)
import Text.Megaparsec.Char (space1)
import Prelude hiding (readFile)

pShebang :: Parser Text
pShebang = chunk "#!/usr/bin/env magix"

pMagix :: Parser Magix
pMagix = pShebang *> space1 *> (MHaskellMagix <$> pHaskellMagix)

getDirectives :: FilePath -> IO Magix
getDirectives x = do
  contents <- readFile x
  let magix = either (error . errorBundlePretty) id $ parse pMagix x contents
  pure magix
