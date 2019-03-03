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
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

import           Control.Applicative      ((<|>))
import           Control.Arrow            as Arrow
import           Control.Exception        (bracket)
import           Control.Monad            (when)
import           Control.Monad.IO.Class   (liftIO)
import           Control.Monad.Reader     (runReaderT)
import qualified Data.Aeson.Encode.Pretty as Json
import qualified Data.ByteString.Lazy     as BL
import qualified Data.HashMap.Strict      as HM
import           Data.Maybe               (fromMaybe)
import           Data.Monoid              ((<>))
import qualified Data.Text                as T
import qualified Data.Text.IO             as T
import           Data.Time.Calendar       (fromGregorian)
import           Data.Time.Clock          (UTCTime (UTCTime), getCurrentTime)
import           Data.Traversable         (for)
import           Safe                     (headMay)
import qualified System.Directory         as Dir
import           System.Envy              (FromEnv, decodeEnv, env, fromEnv)
import           System.FilePath          ((</>))
import           System.IO                (BufferMode (NoBuffering), hGetEcho,
                                           hPrint, hPutStrLn, hSetBuffering,
                                           hSetEcho, stderr, stdin, stdout)
import qualified System.Process.Typed     as P
import qualified Web.Slack                as Slack
import qualified Web.Slack.Channel        as Channel
import qualified Web.Slack.Common         as Slack
import qualified Web.Slack.Group          as Group
import qualified Web.Slack.User           as User

import           SlackLog.Pagination      (chooseLatestPageOf, defaultPageSize,
                                           paginateFiles)
import           SlackLog.Types           (ChannelId, ChannelName,
                                           Visibility (Private, Public),
                                           targetChannels)
import           SlackLog.Util            (failWhenLeft, readJsonFile)
import           Web.Slack.Instances      ()


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

type TimestampsByChannel = HM.HashMap ChannelId Slack.SlackTimestamp


main :: IO ()
main = do
  hSetBuffering stdin  NoBuffering
  hSetBuffering stdout NoBuffering
  hSetBuffering stderr NoBuffering

  apiConfig <- Slack.mkSlackConfig =<< slackApiToken <$> (failWhenLeft =<< decodeEnv)
  tss <- readJsonFile ".timestamps.json" -- TODO: move to doc/json
  logConfig <- readJsonFile "doc/.config.json"
  let targets = targetChannels logConfig
  newTss <- HM.fromList
    <$> for (HM.toList targets) (uncurry $ saveChannel apiConfig tss)

  BL.writeFile ".timestamps.json" $ Json.encodePretty newTss

  Slack.channelsList (Channel.ListReq (Just True) (Just False))
    `runReaderT` apiConfig >>= \case
      Right (Channel.ListRsp chs) -> do
        let channelsByName = HM.fromList $ map ((,) <$> Channel.channelId <*> Channel.channelName) chs
        BL.writeFile "doc/json/.channels.json" $ Json.encodePretty channelsByName
      Left err -> do
        hPutStrLn stderr "WARNING: Error when fetching the list of channels:"
        hPrint stderr err

  Slack.groupsList
    `runReaderT` apiConfig >>= \case
      Right (Group.ListRsp chs) -> do
        -- Groups (private channels) should be saved only
        -- if specified in 'targetChannels' to hide their group names.
        let groupsByName =
              HM.fromList
                . filter ((`HM.member` targets) . fst)
                $ map ((,) <$> Group.groupId <*> Group.groupName) chs
        -- Save groups separately from channels to avoid to save empty hash when error.
        BL.writeFile "doc/json/.groups.json" $ Json.encodePretty groupsByName
      Left err -> do
        hPutStrLn stderr "WARNING: Error when fetching the list of channels:"
        hPrint stderr err

  Slack.usersList
    `runReaderT` apiConfig >>= \case
      Right (User.ListRsp us) -> do
        let usersByName = HM.fromList $ map ((,) <$> Slack.unUserId . User.userId <*> User.userName) us
        BL.writeFile "doc/json/.users.json" $ Json.encodePretty usersByName
      Left err -> do
        hPutStrLn stderr "WARNING: Error when fetching the list of users:"
        hPrint stderr err

  when (tss /= newTss) gitPushMessageLog


saveChannel :: Slack.SlackConfig -> TimestampsByChannel -> ChannelName -> Visibility -> IO (T.Text, Slack.SlackTimestamp)
saveChannel cfg tss channelName vis = Dir.withCurrentDirectory "doc" $ do
  new <- Slack.mkSlackTimestamp <$> getCurrentTime
  let old = fromMaybe (Slack.mkSlackTimestamp $ UTCTime (fromGregorian 2017 1 1) 0) (HM.lookup channelName tss)
  print old
  let hist =
        case vis of
            Private -> Slack.groupsHistory
            Public  -> Slack.channelsHistory
  res <- runReaderT (Slack.historyFetchAll hist channelName 100 old new) cfg
  case res of
      Right body -> do
        let msgs = Slack.historyRspMessages body
            mbLatestTs = Slack.messageTs <$> headMay msgs
        case mbLatestTs of
            Just latestTs -> do
              Dir.withCurrentDirectory "json" $ do
                let channelNameS = T.unpack channelName
                    tmpFileName = channelNameS <> "-tmp.json"
                BL.writeFile tmpFileName $ Json.encodePretty (reverse msgs)
                Dir.createDirectoryIfMissing False channelNameS
                channelDirItems <- Dir.listDirectory channelNameS
                (latestPageFileNames, basePageNum) <-
                  if null channelDirItems
                    then return ([], 1)
                    else Arrow.first ((: []) . (channelNameS </>)) <$> chooseLatestPageOf channelDirItems
                paginateFiles defaultPageSize basePageNum channelNameS (latestPageFileNames ++ [tmpFileName])
                Dir.removeFile tmpFileName
              -- TODO: Convert the updated JSON file to HTML here.
              return (channelName, latestTs)
            _ -> do
              hPutStrLn stderr $ "WARNING: Error when fetching the history of " ++ show channelName ++ ": " ++ "Can't get latest timestamp!"
              return (channelName, old)
      Left err -> do
        hPutStrLn stderr $ "WARNING: Error when fetching the history of " ++ show channelName ++ ":"
        hPrint stderr err
        return (channelName, old)


gitPushMessageLog :: IO ()
gitPushMessageLog = do
  P.runProcess_ $ P.proc "git" ["add", "./.timestamps.json", "./doc/"]
  now <- getCurrentTime
  P.runProcess_ $ P.proc "git" ["commit", "-m", "Slack log update at " ++ show now]
  P.runProcess_ $ P.proc "git" ["push"]
