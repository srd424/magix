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
  ( getBashNixExpression,
  )
where

import Data.Foldable (Foldable (..))
import Data.Text (Text, pack, replace, unwords)
import Data.Text.IO (readFile)
import Magix.Config (Config (..))
import Magix.Languages.Bash.Directives (BashDirectives (..))
import Paths_magix (getDataFileName)
import Prelude hiding (readFile, unwords)

replace' :: Text -> (Text, Text) -> Text
replace' t (x, y) = replace x y t

getBashNixExpression :: Config -> BashDirectives -> IO Text
getBashNixExpression c (BashDirectives ps) = do
  f <- getDataFileName "src/Magix/Languages/Bash/Template.nix"
  e <- readFile f
  let rs =
        [ ("__SCRIPT_NAME__", pack $ scriptName c),
          ("__SCRIPT_SOURCE__", pack $ scriptLinkPath c),
          ("__RUNTIME_INPUTS__", unwords ps)
        ]
  pure $ foldl' replace' e rs
