{-# LANGUAGE OverloadedStrings #-}

module SlackLog.HtmlSpec
  ( spec
  ) where


import qualified Data.ByteString.Lazy    as BL
import qualified Data.Text.Lazy          as TL
import qualified Data.Text.Lazy.Encoding as TLE
import qualified Data.Text.Lazy.IO       as TLI
import qualified System.Directory        as Dir
import           Test.Hspec

import           SlackLog.Html


spec :: Spec
spec = do
  w <- runIO $ loadWorkspaceInfo "test/assets"
  describe "renderSlackMessages" $
    it "converts messages in Slack into a byte string of HTML" $ do
      let p = PageInfo
            { currentPagePath  = "/html/35.html"
            , previousPagePath = Just "/html/34.html"
            , nextPagePath     = Just "/html/36.html"
            , channelId        = "id_of_random"
            }

      expected <- readAsExpectedHtml "test/assets/expected-messages.html"
      Dir.withCurrentDirectory "test/assets" $
        renderSlackMessages w p `shouldReturn` expected

  describe "renderIndexOfPages" $
    it "build index HTML of HTML pages." $ do
      expected <- readAsExpectedHtml "test/assets/expected-index.html"
      let channelAndPaths = [("id_of_random", ["test/assets/35.json"])]

      Dir.withCurrentDirectory "test/assets" $
        renderIndexOfPages w channelAndPaths `shouldReturn` expected


readAsExpectedHtml :: FilePath -> IO BL.ByteString
readAsExpectedHtml =
  fmap (TLE.encodeUtf8 . TL.concat . map TL.strip . TL.lines) . TLI.readFile
