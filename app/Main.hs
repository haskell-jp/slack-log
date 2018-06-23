{-
Copyright 2018 Japan Haskell User Group

Licensed under the Apache License, Version 2.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.
-}

{-|
  Export and upload messages from haskell-jp.slack.com
-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

import           Control.Applicative      ((<|>))
import           Control.Exception        (bracket)
import           Control.Monad.IO.Class   (liftIO)
import           Control.Monad.Reader     (runReaderT)
import qualified Data.Aeson               as Json
import qualified Data.Aeson.Encode.Pretty as Json
import qualified Data.ByteString.Lazy     as BL
import           Data.Foldable            (for_)
import qualified Data.HashMap.Strict      as HM
import           Data.Maybe               (fromMaybe)
import           Data.Monoid              ((<>))
import qualified Data.Text                as T
import qualified Data.Text.IO             as T
import           Data.Time.Calendar       (fromGregorian)
import           Data.Time.Clock          (UTCTime (UTCTime), getCurrentTime)
import           System.Envy              (FromEnv, decodeEnv, env, fromEnv)
import           System.IO                (BufferMode (NoBuffering), hGetEcho,
                                           hPrint, hPutStrLn, hSetBuffering,
                                           hSetEcho, stderr, stdin, stdout)
import qualified Web.Slack                as Slack
import qualified Web.Slack.Common         as Slack


newtype EnvArgs = EnvArgs { slackApiToken :: T.Text } deriving (Eq, Show)

instance FromEnv EnvArgs where
  fromEnv =
    EnvArgs <$> (env "SLACK_API_TOKEN" <|> readHidden)
    where
      readHidden = liftIO $ do
        putStr "SLACK_API_TOKEN: "
        t <- bracket
          (hGetEcho stdin)
          (hSetEcho stdin)
          (const (hSetEcho stdin False >> T.getLine))
        putStrLn ""
        return t


targetChannels :: [T.Text]
targetChannels =
  [ "C8KBGEBR7" -- code-review
  , "C4P499EPQ" -- english
  , "C4LFB6DE0" -- general
  , "CAXQ09PN2" -- haskell-day
  , "C7Y71415W" -- math
  , "C5666B6BB" -- questions
  , "C4M4TT8JJ" -- random
  , "C8R0H137H" -- translation
  ]


type TimestampsByChannel = HM.HashMap T.Text Slack.SlackTimestamp


instance Json.ToJSON Slack.MessageType where
  toJSON _ = Json.String "message"
instance Json.ToJSON Slack.UserId
instance Json.ToJSON Slack.SlackMessageText
instance Json.ToJSON Slack.Message
instance Json.ToJSON Slack.HistoryRsp
instance Json.ToJSON Slack.SlackTimestamp where
  toJSON = Json.String . Slack.slackTimestampTs


main :: IO ()
main = do
  hSetBuffering stdin  NoBuffering
  hSetBuffering stdout NoBuffering
  hSetBuffering stderr NoBuffering

  config <- Slack.mkSlackConfig =<< slackApiToken <$> (failWhenLeft =<< decodeEnv)
  tss <- readLastTimestampsOrDefault ".timestamps.json"
  for_ targetChannels (saveChannel config tss)


readLastTimestampsOrDefault :: FilePath -> IO TimestampsByChannel
readLastTimestampsOrDefault path = do
  result <- Json.eitherDecode <$> BL.readFile path
  case result of
      Right tbc -> return tbc
      Left err  -> fail $ "Error reading \"" ++ path ++ "\"\n" ++ show err


saveChannel :: Slack.SlackConfig -> TimestampsByChannel -> T.Text -> IO ()
saveChannel cfg tss channelName = do
  new <- Slack.mkSlackTimestamp <$> getCurrentTime
  let old = fromMaybe (Slack.mkSlackTimestamp $ UTCTime (fromGregorian 2017 1 1) 0) (HM.lookup channelName tss)
  print old
  res <- runReaderT (Slack.historyFetchAll Slack.channelsHistory channelName 100 old new) cfg
  case res of
      Right js -> do
        -- TODO: save .timestamps.json
        BL.writeFile ("doc/json/" <> T.unpack channelName <> ".json") $ Json.encodePretty js
      Left err -> do
        hPutStrLn stderr $ "WARNING: Error when fetching the history of " ++ show channelName ++ ":"
        hPrint stderr err


failWhenLeft :: Either String a -> IO a
failWhenLeft = either fail return
