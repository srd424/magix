-- |
-- Module      :  Magix.Run
-- Description :  Run the built executable
-- Copyright   :  2024 Dominik Schrempf
-- License     :  GPL-3.0-or-later
--
-- Maintainer  :  dominik.schrempf@gmail.com
-- Stability   :  experimental
-- Portability :  portable
--
-- Creation date: Sun Oct 20 10:53:12 2024.
module Magix.Run
  ( run,
  )
where

import Magix.Config (Config (scriptName))
import Magix.Options (Options (scriptArgs))
import Magix.Paths (getResultDir)
import System.FilePath ((</>))
import System.Process (callProcess)

run :: Options -> Config -> IO ()
run opts conf = do
  resultDir <- getResultDir conf
  let executable = resultDir </> "bin" </> scriptName conf
  callProcess executable (scriptArgs opts)
