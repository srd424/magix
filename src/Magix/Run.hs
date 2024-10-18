-- |
-- Module      :  Magix.Run
-- Description :  Run `nix-script`
-- Copyright   :  2024 Dominik Schrempf
-- License     :  GPL-3.0-or-later
--
-- Maintainer  :  dominik.schrempf@gmail.com
-- Stability   :  experimental
-- Portability :  portable
--
-- Creation date: Fri Oct 18 15:19:57 2024.
module Magix.Run
  ( runMagix,
  )
where

import Magix.Builder (buildArgs)
import Magix.Magix (Magix)
import Magix.Options (MagixOptions)
import System.Process (callProcess)

runMagix :: MagixOptions -> Magix -> IO ()
runMagix o m = callProcess "nix-script" args
  where
    args = buildArgs o m
