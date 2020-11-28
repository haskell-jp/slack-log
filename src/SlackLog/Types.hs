{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}
{-# LANGUAGE StrictData     #-}

module SlackLog.Types where

import qualified Data.Aeson          as Json
import qualified Data.HashMap.Strict as HM
import qualified Data.Text           as T
import           GHC.Generics        (Generic)

import           SlackLog.Duration


-- | Configuration type used mainly when converting JSON files into HTML.
data Config = Config
  { workspaceName     :: T.Text
  , timeZone          :: String
  -- ^ Show the times on this timezone.
  , saveRepliesBefore :: Duration
  -- ^ How long slack-logger saves replies before.
  --   slack-logger checks if a thread has more replies for every saved parent
  --   message if the 'Web.Slack.Common.messageTs' of the last reply of the
  --   thread is after 'saveRepliesBefore'.
  --   Here are valid values for example:
  --
  --   * @1m@ for 1 minute.
  --   * @2h@ for 2 hours.
  --   * @3d@ for 3 days.
  --   * @4w@ for 4 weeks.
  , targetChannels    :: TargetChannels
  -- ^ Target channels whose messages are collected by slack-logger.
  --   This is the only configuration item used when collecting JSON files
  --   from Slack: not only when converting JSON files into HTML.
  } deriving (Eq, Show, Generic, Json.FromJSON)

data TargetChannel = TargetChannel
  { visibility :: Visibility
  , label      :: T.Text
  } deriving (Show, Eq, Generic)
instance Json.FromJSON TargetChannel

data Visibility = Private | Public deriving (Eq, Show, Generic)
instance Json.FromJSON Visibility

type TargetChannels = HM.HashMap ChannelId TargetChannel

type UserName = T.Text

type UserId = T.Text

type ChannelName = T.Text

type ChannelId = T.Text
