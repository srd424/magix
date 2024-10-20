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

import Magix.Build (getResultDir)
import Magix.Config (Config (scriptName))
import System.FilePath ((</>))
import System.Process (callProcess)

run :: Config -> IO ()
run c = do
  resultDir <- getResultDir c
  let executable = resultDir </> "bin" </> scriptName c
  callProcess executable []
