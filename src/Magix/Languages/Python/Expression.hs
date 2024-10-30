-- |
-- Module      :  Magix.Languages.Python.Expression
-- Description :  Build Python command lines
-- Copyright   :  2024 Dominik Schrempf
-- License     :  GPL-3.0-or-later
--
-- Maintainer  :  dominik.schrempf@gmail.com
-- Stability   :  experimental
-- Portability :  portable
--
-- Creation date: Fri Oct 18 13:36:32 2024.
module Magix.Languages.Python.Expression
  ( getPythonReplacements,
  )
where

import Data.Text (Text, unwords)
import Magix.Languages.Python.Directives (PythonDirectives (..))
import Prelude hiding (unwords)

getPythonReplacements :: PythonDirectives -> [(Text, Text)]
getPythonReplacements (PythonDirectives ps) = [("__PYTHON_PACKAGES__", unwords ps)]
