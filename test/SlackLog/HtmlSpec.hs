{-# LANGUAGE OverloadedStrings #-}

module SlackLog.HtmlSpec
  ( spec
  ) where


import qualified Data.ByteString    as B
import           Data.List          (isSuffixOf)
import qualified Data.Text          as T
import qualified Data.Text.Encoding as TE
import qualified Data.Yaml          as Yaml
import qualified System.Directory   as Dir
import           Test.Hspec

import           SlackLog.Html


spec :: Spec
spec = do
  w <- runIO $ do
    config <- Yaml.decodeFileThrow "slack-log.yaml"
    loadWorkspaceInfo config "test/assets"

  let idOfRandom = "C4M4TT8JJ"
  -- The random channel configured in slack-log.yaml

  describe "renderSlackMessages" $
    it "converts messages in Slack into a byte string of HTML" $ do
      let p = PageInfo
            { currentPagePath  = "/html/35.html"
            , previousPagePath = Just "/html/34.html"
            , nextPagePath     = Just "/html/36.html"
            , channelId        = idOfRandom
            }

      expected <- readAsExpectedHtml "test/assets/expected-messages.html"
      Dir.withCurrentDirectory "test/assets" $
        renderSlackMessages w p `shouldReturn` expected

  describe "renderIndexOfPages" $
    it "build index HTML of HTML pages." $ do
      expected <- readAsExpectedHtml "test/assets/expected-index.html"
      let channelAndPaths = [(idOfRandom, ["test/assets/35.json"])]

      Dir.withCurrentDirectory "test/assets" $
        renderIndexOfPages w channelAndPaths `shouldReturn` expected

  describe "collectTargetJsons" $
    it "Returns JSON files for files under the docs/json directory" $
      Dir.withCurrentDirectory "docs" $ do
        result <- collectTargetJsons idOfRandom
        result `shouldSatisfy` all (".json" `isSuffixOf`)


readAsExpectedHtml :: FilePath -> IO T.Text
readAsExpectedHtml = fmap TE.decodeUtf8 . B.readFile
