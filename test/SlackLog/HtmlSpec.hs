{-# LANGUAGE OverloadedStrings #-}

module SlackLog.HtmlSpec
  ( spec
  ) where


import qualified Data.Aeson              as Json
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text.Lazy          as TL
import qualified Data.Text.Lazy.Encoding as TLE
import qualified Data.Text.Lazy.IO       as TLI
import           Test.Hspec

import           SlackLog.Html


spec :: Spec
spec = do
  describe "renderSlackMessages" $
    it "converts messages in Slack into a byte string of HTML" $ do
      let p = PageInfo
            { pageNumber       = 35
            , previousPagePath = Just "/html/34.html"
            , nextPagePath     = Just "/html/36.html"
            , channelId        = "id_of_random"
            }

      w <- loadWorkspaceInfo "test/assets"

      expected <- readAsExpectedHtml "test/assets/expected-messages.html"
      Just msgs <- Json.decodeFileStrict' "test/assets/testMessages.json"

      renderSlackMessages w p msgs `shouldBe` expected

  describe "renderIndexOfPages" $
    it "build index HTML of HTML pages." $ do
      expected <- readAsExpectedHtml "test/assets/expected-index.html"
      w <- loadWorkspaceInfo "test/assets"
      let channelAndPaths = [("id_of_random", "test/assets/testMessages.json")]
      renderIndexOfPages w channelAndPaths `shouldReturn` expected


readAsExpectedHtml :: FilePath -> IO BL.ByteString
readAsExpectedHtml =
  fmap (TLE.encodeUtf8 . TL.concat . map TL.strip . TL.lines) . TLI.readFile
