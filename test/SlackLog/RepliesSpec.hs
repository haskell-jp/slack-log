{-# LANGUAGE OverloadedStrings #-}

module SlackLog.RepliesSpec
  ( spec
  ) where

import           Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import           System.Directory      (withCurrentDirectory)
import           System.FilePath       ((</>))
import           Test.Hspec
import           Web.Slack.Common      (ConversationId (..), Message (..),
                                        MessageType (..), SlackMessageText (..),
                                        UserId (..), mkSlackTimestamp)

import           SlackLog.Replies

{-
  観点: スレッドの先頭のメッセージのタイムスタンプが、saveSince以前
    観点: スレッドが空（replyがない）の場合
    観点: スレッドにreplyがある場合
      観点: 最後のreplyタイムスタンプが、saveSinceより後
      観点: 最後のreplyタイムスタンプが、saveSince以前
  観点: スレッドの先頭のメッセージのタイムスタンプが、saveSinceより後
    観点: スレッドが空（replyがない）の場合
    観点: スレッドにreplyがある場合
      観点: 最後のreplyタイムスタンプが、saveSinceより後
-}


spec :: Spec
spec =
  describe "searchThreadsAppendedSince" $
    it "returns threads in the conversation whose first message or last is created since the first argument's time" $
      withCurrentDirectory "test/assets/json" $ do
        -- The timestamp of the 3rd test message
        let saveSince = posixSecondsToUTCTime 1547694883.115200
            conversationId = ConversationId "C4M4TT8AA"
        actual <- searchThreadsAppendedSince saveSince conversationId
        let expected =
              [ Thread
                { tFirstMessage = Message
                  { messageType = MessageTypeMessage
                  , messageUser = Just $ UserId "U11111"
                  , messageText = SlackMessageText "Test `searchThreadsAppendedSince`: the first message is after saveSince, and the last message is after saveSince."
                  , messageTs = mkSlackTimestamp $ posixSecondsToUTCTime 1547694884.115200
                  }
                , tLatestTs = mkSlackTimestamp $ posixSecondsToUTCTime 1547694994.115200
                , tMessages =
                  [ Message
                    { messageType = MessageTypeMessage
                    , messageUser = Just $ UserId "U11111"
                    , messageText = SlackMessageText "4-1 message"
                    , messageTs = mkSlackTimestamp $ posixSecondsToUTCTime 1547694994.115200
                    }
                  ]
                , tPath = "2" </> "1547694884.115200.json"
                }
              , Thread
                { tFirstMessage = Message
                  { messageType = MessageTypeMessage
                  , messageUser = Just $ UserId "U12345"
                  , messageText = SlackMessageText "Test `searchThreadsAppendedSince`: the first message is after saveSince and has no replies."
                  , messageTs = mkSlackTimestamp $ posixSecondsToUTCTime 1547694885.115200
                  }
                , tLatestTs = mkSlackTimestamp $ posixSecondsToUTCTime 1547694885.115200
                , tMessages = []
                , tPath = "2" </> "1547694885.115200.json"
                }
              , Thread
                { tFirstMessage = Message
                  { messageType = MessageTypeMessage
                  , messageUser = Just $ UserId "U33333"
                  , messageText = SlackMessageText "Test `searchThreadsAppendedSince`: the first message is before saveSince, and the last message is after saveSince."
                  , messageTs = mkSlackTimestamp $ posixSecondsToUTCTime 1547694883.115200
                  }
                , tLatestTs = mkSlackTimestamp $ posixSecondsToUTCTime 1547694883.315200
                , tMessages =
                  [ Message
                    { messageType = MessageTypeMessage
                    , messageUser = Just $ UserId "U32345"
                    , messageText = SlackMessageText "3-1 message"
                    , messageTs = mkSlackTimestamp $ posixSecondsToUTCTime 1547694883.215200
                    }
                  , Message
                    { messageType = MessageTypeMessage
                    , messageUser = Just $ UserId "U32356"
                    , messageText = SlackMessageText "3-2 message"
                    , messageTs = mkSlackTimestamp $ posixSecondsToUTCTime 1547694883.315200
                    }
                  ]
                , tPath = "1" </> "1547694883.115200.json"
                }
              ]
        actual `shouldBe` expected
