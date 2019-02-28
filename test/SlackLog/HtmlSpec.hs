{-# LANGUAGE OverloadedStrings #-}

module SlackLog.HtmlSpec
  ( spec
  ) where


import qualified Data.Aeson              as Json
import qualified Data.HashMap.Strict     as HM
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

          w = WorkspaceInfo
            { userNameById      = HM.fromList [("U12345", "example_user")]
            , channelNameById   = HM.fromList [("id_of_random", "random")]
            , workspaceInfoName = "haskell-jp"
            }

      expected <-
        TLE.encodeUtf8
          .   TL.concat
          .   map TL.strip
          .   TL.lines
          <$> TLI.readFile "test/assets/expected.html"
      Just msgs <- Json.decodeFileStrict' "test/assets/test.json"

      renderSlackMessages w p msgs `shouldBe` expected
