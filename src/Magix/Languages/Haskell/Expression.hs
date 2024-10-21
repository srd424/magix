-- |
-- Module      :  Magix.Languages.Haskell.Expression
-- Description :  Build Haskell command lines
-- Copyright   :  2024 Dominik Schrempf
-- License     :  GPL-3.0-or-later
--
-- Maintainer  :  dominik.schrempf@gmail.com
-- Stability   :  experimental
-- Portability :  portable
--
-- Creation date: Fri Oct 18 13:36:32 2024.
module Magix.Languages.Haskell.Expression
  ( getHaskellNixExpression,
  )
where

import Data.Foldable (Foldable (..))
import Data.Text (Text, pack, replace, unwords)
import Data.Text.IO (readFile)
import Magix.Config (Config (..))
import Magix.Languages.Haskell.Directives (HaskellDirectives (..))
import Paths_magix (getDataFileName)
import Prelude hiding (readFile, unwords)

replace' :: Text -> (Text, Text) -> Text
replace' t (x, y) = replace x y t

getHaskellNixExpression :: Config -> HaskellDirectives -> IO Text
getHaskellNixExpression c (HaskellDirectives ps fs) = do
  f <- getDataFileName "src/Magix/Languages/Haskell/Template.nix"
  e <- readFile f
  let rs =
        [ ("__SCRIPT_NAME__", pack $ scriptName c),
          ("__SCRIPT_SOURCE__", pack $ scriptLinkPath c),
          ("__HASKELL_PACKAGES__", unwords ps),
          ("__GHC_FLAGS__", unwords fs)
        ]
  pure $ foldl' replace' e rs
