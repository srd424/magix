-- |
-- Module      :  Directive
-- Description :  Parse directives
-- Copyright   :  2024 Dominik Schrempf
-- License     :  GPL-3.0-or-later
--
-- Maintainer  :  dominik.schrempf@gmail.com
-- Stability   :  experimental
-- Portability :  portable
--
-- Creation date: Fri Oct 18 09:17:40 2024.
module Directive
  ( pShebang,
  )
where

import Data.Functor (($>))
import Data.Text (Text)
import Data.Void (Void)
import Text.Megaparsec (Parsec, chunk)

type Parser = Parsec Void Text

pShebang :: Parser ()
pShebang = chunk "#!/usr/bin/env magix" $> ()
