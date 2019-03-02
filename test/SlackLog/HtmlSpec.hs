{-# LANGUAGE OverloadedStrings #-}

module SlackLog.HtmlSpec
  ( spec
  ) where


import qualified Data.Aeson              as Json
import qualified Data.Text.Lazy          as TL
import qualified Data.Text.Lazy.Encoding as TLE
import qualified Data.Text.Lazy.IO       as TLI
import           Test.Hspec

import           SlackLog.Html


spec :: Spec
spec =
  describe "renderSlackMessages" $
    it "converts messages in Slack into a byte string of HTML" $ do
      let p = PageInfo
            { pageNumber       = 35
            , previousPagePath = Just "/html/34.html"
            , nextPagePath     = Just "/html/36.html"
            , channelId        = "id_of_random"
            }

      w <- loadWorkspaceInfo "test/assets"

      expected <-
        TLE.encodeUtf8
          .   TL.concat
          .   map TL.strip
          .   TL.lines
          <$> TLI.readFile "test/assets/expected.html"
      Just msgs <- Json.decodeFileStrict' "test/assets/testMessage.json"

      renderSlackMessages w p msgs `shouldBe` expected
