-- |
-- Module      :  Magix.Build
-- Description :  Build `nix-script` command line
-- Copyright   :  2024 Dominik Schrempf
-- License     :  GPL-3.0-or-later
--
-- Maintainer  :  dominik.schrempf@gmail.com
-- Stability   :  experimental
-- Portability :  portable
--
-- Creation date: Fri Oct 18 13:36:32 2024.
module Magix.Build () where

-- nix-script --build-command 'mv $SRC $SRC.hs; ghc -threaded -o $OUT $SRC.hs' --build-input 'haskellPackages.ghcWithPackages (ps: with ps; [ containers text turtle ])' /home/dominik/bin/nix/nix-run-rofi
