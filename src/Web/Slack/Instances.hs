{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Web.Slack.Instances where


import qualified Data.Aeson                as Json
import           Data.Maybe                (fromJust)
import qualified Data.Text                 as T
import           Data.Time.Clock.POSIX     (posixSecondsToUTCTime)
import           Data.Word                 (Word)
import qualified Test.QuickCheck           as Q
import           Test.QuickCheck.Instances ()
import qualified Web.Slack.Common          as Slack
import           Web.Slack.Instances.Lib   (deriveToJsonWithoutTypeNamePrefixUnderscore)


instance Json.ToJSON Slack.MessageType where
  toJSON _ = Json.String "message"
instance Q.Arbitrary Slack.MessageType where
  arbitrary = return Slack.MessageTypeMessage

instance Json.ToJSON Slack.UserId where
  toJSON = Json.toJSON . Slack.unUserId
instance Q.Arbitrary Slack.UserId where
  arbitrary = fromJust . Json.decode . Json.encode <$> (Q.arbitrary :: Q.Gen T.Text)


deriving instance Json.ToJSON Slack.SlackMessageText
deriving instance Q.Arbitrary Slack.SlackMessageText


instance Json.ToJSON Slack.SlackTimestamp where
  toJSON = Json.String . Slack.slackTimestampTs
instance Q.Arbitrary Slack.SlackTimestamp where
  -- NOTE: `Q.arbitrary :: Q.Gen UTCTime` only for positive POSIX Second is really slow!
  arbitrary = Slack.mkSlackTimestamp . posixSecondsToUTCTime . fromIntegral <$> (Q.arbitrary :: Q.Gen Word)


$(deriveToJsonWithoutTypeNamePrefixUnderscore ''Slack.Message)
instance Q.Arbitrary Slack.Message where
  arbitrary =
    Slack.Message <$> Q.arbitrary <*> Q.arbitrary <*> Q.arbitrary <*> Q.arbitrary
