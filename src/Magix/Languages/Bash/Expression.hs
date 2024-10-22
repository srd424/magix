-- |
-- Module      :  Magix.Languages.Bash.Expression
-- Description :  Build Bash command lines
-- Copyright   :  2024 Dominik Schrempf
-- License     :  GPL-3.0-or-later
--
-- Maintainer  :  dominik.schrempf@gmail.com
-- Stability   :  experimental
-- Portability :  portable
--
-- Creation date: Fri Oct 18 13:36:32 2024.
module Magix.Languages.Bash.Expression
  ( getBashReplacements,
  )
where

import Data.Text (Text, unwords)
import Magix.Languages.Bash.Directives (BashDirectives (..))
import Prelude hiding (unwords)

getBashReplacements :: BashDirectives -> [(Text, Text)]
getBashReplacements (BashDirectives ps) = [("__RUNTIME_INPUTS__", unwords ps)]
