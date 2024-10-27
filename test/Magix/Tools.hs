-- |
-- Module      :  Magix.Tools
-- Description :  Helpers shared across tests
-- Copyright   :  2024 Dominik Schrempf
-- License     :  GPL-3.0-or-later
--
-- Maintainer  :  dominik.schrempf@gmail.com
-- Stability   :  experimental
-- Portability :  portable
--
-- Creation date: Mon Oct 21 21:56:26 2024.
module Magix.Tools
  ( doesNotContainTemplates,
    containsSpaceSeparatedValues,
    getRandomFakeConfig,
  )
where

import Control.Monad (replicateM)
import Data.Text (Text, isInfixOf, unwords)
import Magix.Config (Config (..))
import System.Directory (createDirectory, getTemporaryDirectory)
import System.FilePath ((</>))
import System.Random.Stateful (randomIO, randomRIO)
import Prelude hiding (unwords)

doesNotContainTemplates :: Text -> Bool
doesNotContainTemplates = not . isInfixOf "__"

containsSpaceSeparatedValues :: [Text] -> Text -> Bool
containsSpaceSeparatedValues xs = isInfixOf (unwords xs)

getRandomString :: IO String
getRandomString = replicateM 40 $ randomRIO ('a', 'z')

getRandomFakeConfig :: IO Config
getRandomFakeConfig = do
  dir <- ("magix-" <>) <$> getRandomString
  tmp <- (</> dir) <$> getTemporaryDirectory
  createDirectory tmp
  hsh <- abs <$> (randomIO :: IO Int)
  pure $
    Config
      (tmp </> "fakeScriptPath")
      (tmp </> "fakeScriptName")
      (tmp </> "fakeNixpkgsPath")
      hsh
      (tmp </> "fakeCacheDir")
      (tmp </> "fakeScriptLinkPath")
      (tmp </> "fakeBuildDir")
      (tmp </> "fakeBuildExprPath")
      (tmp </> "fakeResultDir")
