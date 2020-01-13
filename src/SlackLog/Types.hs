{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData        #-}

module SlackLog.Types where

import qualified Data.Aeson          as Json
import qualified Data.Aeson.Types    as JsonTypes
import qualified Data.HashMap.Strict as HM
import qualified Data.Text           as T
import           GHC.Generics        (Generic)
import           Safe                (headMay)


-- | Configuration type used mainly when converting JSON files into HTML.
data Config = Config
  { workspaceName  :: T.Text
  , timeZone       :: String
  -- ^ Show the times on this timezone.
  , targetChannels :: TargetChannels
  -- ^ Target channels whose messages are collected by slack-logger.
  --   This is the only configuration item used when collecting JSON files
  --   from Slack: not only when converting JSON files into HTML.
  } deriving (Eq, Show, Generic, Json.FromJSON)

data TargetChannel = TargetChannel
  { visibility :: Visibility
  , label :: T.Text
  } deriving (Show, Eq, Generic)
instance Json.FromJSON TargetChannel

data Visibility = Private | Public deriving (Eq, Show, Generic)
instance Json.FromJSON Visibility

type TargetChannels = HM.HashMap ChannelId TargetChannel

type UserName = T.Text

type UserId = T.Text

type ChannelName = T.Text

type ChannelId = T.Text
