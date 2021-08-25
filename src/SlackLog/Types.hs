{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}
{-# LANGUAGE StrictData     #-}

module SlackLog.Types
  ( module SlackLog.Duration
  , Config (..)
  , TemplatePaths (..)
  , TargetChannels
  , UserName
  , UserId
  , ChannelName
  , ChannelId
  ) where

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
  , templatePaths     :: TemplatePaths
  -- ^ Path to the mustache template file used to convert the JSON files
  --   into HTML. The template engine is <https://hackage.haskell.org/package/mustache mustache>.
  } deriving (Eq, Show, Generic, Json.FromJSON)


data TemplatePaths = TemplatePaths
  { indexPage    :: FilePath
  , messagesPage :: FilePath
  , threadPage   :: FilePath
  } deriving (Eq, Show, Generic, Json.FromJSON)

type TargetChannels = HM.HashMap ChannelId ChannelName

type UserName = T.Text

type UserId = T.Text

type ChannelId = T.Text

type ChannelName = T.Text
