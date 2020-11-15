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

-- | Export and upload messages from haskell-jp.slack.com

{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Applicative      ((<|>))
import           Control.Arrow            as Arrow
import           Control.Exception        (bracket)
import           Control.Monad            (unless, when)
import           Control.Monad.IO.Class   (liftIO)
import           Control.Monad.Reader     (runReaderT)
import qualified Data.Aeson.Encode.Pretty as Json
import qualified Data.ByteString.Lazy     as BL
import qualified Data.HashMap.Strict      as HM
import           Data.Maybe               (fromMaybe, maybeToList)
import qualified Data.Text                as T
import qualified Data.Text.IO             as T
import           Data.Time.Calendar       (fromGregorian)
import           Data.Time.Clock          (UTCTime (UTCTime), getCurrentTime)
import           Data.Traversable         (for)
import           Data.Yaml                as Yaml
import           Safe                     (headMay)
import qualified System.Directory         as Dir
import           System.Envy              (FromEnv, decodeEnv, env, fromEnv)
import           System.Exit              (die)
import           System.FilePath          ((</>))
import           System.IO                (BufferMode (NoBuffering), hGetEcho,
                                           hPrint, hPutStrLn, hSetBuffering,
                                           hSetEcho, stderr, stdin, stdout)
import qualified Web.Slack                as Slack
import qualified Web.Slack.Common         as Slack
import qualified Web.Slack.Group          as Group
import qualified Web.Slack.User           as User

import           SlackLog.Html
import           SlackLog.Pagination      (chooseLatestPageOf, defaultPageSize,
                                           paginateFiles)
import           SlackLog.Types           (ChannelId, TargetChannel (..),
                                           TargetChannels,
                                           Visibility (Private, Public),
                                           targetChannels)
import           SlackLog.Util            (failWhenLeft, readJsonFile)
import           UI.Butcher.Monadic       (addCmd, addCmdImpl, addHelpCommand,
                                           addSimpleBoolFlag, flagHelpStr,
                                           mainFromCmdParserWithHelpDesc)
import           Web.Slack.Instances      ()


newtype EnvArgs = EnvArgs { slackApiToken :: T.Text } deriving (Eq, Show)

instance FromEnv EnvArgs where
  fromEnv _ =
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
  mainFromCmdParserWithHelpDesc $ \helpDesc -> do
    addHelpCommand helpDesc
    addCmd "save" $ addCmdImpl saveCmd
    addCmd "generate-html" $ do
      onlyIndex <- addSimpleBoolFlag "i" ["only-index"] $ flagHelpStr "generates only index.html"
      addCmdImpl $ generateHtmlCmd onlyIndex
    addCmd "paginate-json" $ addCmdImpl paginateJsonCmd


saveCmd :: IO ()
saveCmd = do
  config <- Yaml.decodeFileThrow "slack-log.yaml"
  apiConfig <- Slack.mkSlackConfig . slackApiToken =<< failWhenLeft =<< decodeEnv

  Dir.withCurrentDirectory "docs" $ do
    ws <- loadWorkspaceInfo config "json"
    oldTss <- readJsonFile "json/.timestamps.json"
    let targets = targetChannels config

    -- These actions have to be performed before generating HTMLs.
    -- Because generating HTMLs requires channelsByName, usersByName, groupsByName
    saveUsersList apiConfig
    saveGroupsList apiConfig targets

    saveResult <- for (HM.toList targets) $ \(chanId, TargetChannel{visibility=vis}) -> do
      newTs <- saveChannel apiConfig oldTss chanId vis

      jsonPaths <- collectTargetJsons chanId
      convertJsonsInChannel ws chanId jsonPaths

      return ((chanId, newTs), (chanId, jsonPaths))

    let (newTss, newNames) = Arrow.first HM.fromList $ unzip saveResult

    BL.writeFile "json/.timestamps.json" $ Json.encodePretty newTss

    when (oldTss /=  newTss) $
      generateIndexHtml ws newNames


generateHtmlCmd :: Bool -> IO ()
generateHtmlCmd onlyIndex = do
  logConfig <- Yaml.decodeFileThrow "slack-log.yaml"
  Dir.withCurrentDirectory "docs" $ do
    ws <- loadWorkspaceInfo logConfig "json"

    namesByChannel <- for (HM.keys $ targetChannels logConfig) $ \chanId -> do
      jsonPaths <- collectTargetJsons chanId
      unless onlyIndex $
        convertJsonsInChannel ws chanId jsonPaths
      return (chanId, jsonPaths)

    generateIndexHtml ws namesByChannel


paginateJsonCmd :: IO ()
paginateJsonCmd =
  die $ unlines
    [ "Sorry, this feature is currently disabled!"
    , "Related issue: https://github.com/haskell-jp/slack-log/issues/40."
    , "If you're interested in the original source code, see https://github.com/haskell-jp/slack-log/blob/1c155c0ca0860c6a0e8394b8e1a29de6cc00b245/app/paginate-old-jsons.hs"
    ]


saveUsersList :: Slack.SlackConfig -> IO ()
saveUsersList apiConfig =
  Slack.usersList
    `runReaderT` apiConfig >>= \case
      Right (User.ListRsp us) -> do
        let usersByName = HM.fromList $ map ((,) <$> Slack.unUserId . User.userId <*> User.userName) us
        BL.writeFile "json/.users.json" $ Json.encodePretty usersByName
      Left err -> do
        hPutStrLn stderr "WARNING: Error when fetching the list of users:"
        hPrint stderr err

saveGroupsList :: Slack.SlackConfig -> TargetChannels -> IO ()
saveGroupsList apiConfig targets =
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
        BL.writeFile "json/.groups.json" $ Json.encodePretty groupsByName
      Left err -> do
        hPutStrLn stderr "WARNING: Error when fetching the list of channels:"
        hPrint stderr err


saveChannel
  :: Slack.SlackConfig
  -> TimestampsByChannel
  -> ChannelId
  -> Visibility
  -> IO Slack.SlackTimestamp
saveChannel cfg tss chanId vis = do
  new <- Slack.mkSlackTimestamp <$> getCurrentTime
  let old = fromMaybe (Slack.mkSlackTimestamp $ UTCTime (fromGregorian 2017 1 1) 0) (HM.lookup chanId tss)
  print old
  let hist =
        case vis of
            Private -> Slack.groupsHistory
            Public  -> Slack.channelsHistory
  res <- runReaderT (Slack.historyFetchAll hist chanId 100 old new) cfg
  case res of
      Right body -> do
        let msgs = Slack.historyRspMessages body
            mbLatestTs = Slack.messageTs <$> headMay msgs
        case mbLatestTs of
            Just latestTs -> do
              addMessagesToChannelDirectory chanId msgs
              return latestTs
            Nothing -> do
              hPutStrLn stderr $ "WARNING: Error when fetching the history of " ++ show chanId ++ ": " ++ "Can't get latest timestamp!"
              return old
      Left err -> do
        hPutStrLn stderr $ "WARNING: Error when fetching the history of " ++ show chanId ++ ":"
        hPrint stderr err
        return old


addMessagesToChannelDirectory :: ChannelId -> [Slack.Message] -> IO ()
addMessagesToChannelDirectory chanId msgs =
  Dir.withCurrentDirectory "json" $ do
    let channelNameS = T.unpack chanId
        tmpFileName = channelNameS <> "-tmp.json"
    BL.writeFile tmpFileName $ Json.encodePretty (reverse msgs)
    Dir.createDirectoryIfMissing False channelNameS

    channelDirItems <- Dir.listDirectory channelNameS
    (mLatestPageFileName, basePageNum) <-
      if null channelDirItems
        then return (Nothing, 1)
        else Arrow.first (Just . (channelNameS </>)) <$> chooseLatestPageOf channelDirItems
    -- OPTIMIZE: Return updated/created JSON files so that `convertJsonsInChannel` can convert only them.
    --           Current implementation of `convertJsonsInChannel` converts *all* JSON files anytime when
    --           messages are fetched.
    paginateFiles defaultPageSize basePageNum channelNameS (maybeToList mLatestPageFileName ++ [tmpFileName])
    Dir.removeFile tmpFileName
