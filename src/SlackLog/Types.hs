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
  , rootPath       :: Maybe String
  -- ^ Any paths in the generated HTML files are prefixed with this path.
  --   Default: "/"
  , timeZone       :: String
  -- ^ Show the times on this timezone.
  , targetChannels :: TargetChannels
  -- ^ Target channels whose messages are collected by slack-logger.
  --   This is the only configuration item used when collecting JSON files
  --   from Slack: not only when converting JSON files into HTML.
  } deriving (Eq, Show, Generic, Json.FromJSON)


data Visibility = Private | Public deriving (Eq, Show)

instance Json.FromJSON Visibility where
  parseJSON = JsonTypes.withText typ $ \t -> do
    let err = JsonTypes.typeMismatch "Visibility" (JsonTypes.String t)
    -- Parse only the first word of the text to allow users to leave comments.
    firstWord <- maybe err pure . headMay $ T.words t
    case firstWord of
        "Private" -> pure Private
        "Public"  -> pure Public
        _         -> err
   where
    typ = "Visibility"

type TargetChannels = HM.HashMap ChannelId Visibility

type UserName = T.Text

type UserId = T.Text

type ChannelName = T.Text

type ChannelId = T.Text
