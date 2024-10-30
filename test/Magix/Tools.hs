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
  ( getRandomFakeConfig,
    testDirectives,
  )
where

import Control.Monad (replicateM)
import Data.Text (Text, isInfixOf, unwords)
import Magix.Config (Config (..))
import Magix.Directives (Directives, getLanguageName)
import Magix.Expression (getNixExpression, getReplacements, getTemplate)
import System.Directory (createDirectory, getTemporaryDirectory)
import System.FilePath ((</>))
import System.Random.Stateful (randomIO, randomRIO)
import Test.Hspec (Spec, describe, it, shouldBe, shouldSatisfy)
import Prelude hiding (unwords)

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
      (tmp </> "fakeLockPath")
      (tmp </> "fakeScriptLinkPath")
      (tmp </> "fakeBuildDir")
      (tmp </> "fakeBuildExprPath")
      (tmp </> "fakeResultDir")

allReplacementsUsed :: Text -> [(Text, Text)] -> Bool
allReplacementsUsed x = all (\(r, _) -> r `isInfixOf` x)

doesNotContainTemplates :: Text -> Bool
doesNotContainTemplates = not . isInfixOf "__"

containsSpaceSeparatedValues :: [Text] -> Text -> Bool
containsSpaceSeparatedValues xs = isInfixOf (unwords xs)

testDirectives :: Directives -> [[Text]] -> Spec
testDirectives directives values = do
  describe (withName "getReplacements") $ do
    it "all replacements should be used as placeholders in the templates" $ do
      config <- getRandomFakeConfig
      templ <- getTemplate languageName
      allReplacementsUsed templ (getReplacements config directives) `shouldBe` True

  describe (withName "getNixExpression") $ do
    it "all placeholders in templates should be replaced" $ do
      config <- getRandomFakeConfig
      expr <- getNixExpression config directives
      expr `shouldSatisfy` doesNotContainTemplates

  describe (withName "getNixExpression") $ do
    it "works correctly for some sample data" $ do
      config <- getRandomFakeConfig
      expr <- getNixExpression config directives
      sequence_ [expr `shouldSatisfy` containsSpaceSeparatedValues value | value <- values]
  where
    languageName = getLanguageName directives
    withName xs = "[" <> languageName <> "] " <> xs
