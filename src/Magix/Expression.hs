-- |
-- Module      :  Magix.Expression
-- Description :  Get Nix expressions
-- Copyright   :  2024 Dominik Schrempf
-- License     :  GPL-3.0-or-later
--
-- Maintainer  :  dominik.schrempf@gmail.com
-- Stability   :  experimental
-- Portability :  portable
--
-- Creation date: Fri Oct 18 13:36:32 2024.
module Magix.Expression (getNixExpression) where

import Data.Text (Text)
import Magix.Bash.Expression (getBashNixExpression)
import Magix.Config (Config (..))
import Magix.Directives (Directives (..))
import Magix.Haskell.Expression (getHaskellNixExpression)
import Prelude hiding (unwords)

getNixExpression :: Config -> Directives -> IO Text
getNixExpression c (Haskell x) = getHaskellNixExpression c x
getNixExpression c (Bash x) = getBashNixExpression c x
