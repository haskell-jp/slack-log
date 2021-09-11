{-
Copyright 2021 Japan Haskell User Group

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

{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Applicative      ((<|>))
import           Control.Arrow            as Arrow
import           Control.Exception        (bracket)
import           Control.Monad            (unless, when)
import           Control.Monad.Catch      (throwM)
import           Control.Monad.IO.Class   (liftIO)
import           Control.Monad.Reader     (ReaderT, ask, runReaderT)
import qualified Data.Aeson.Encode.Pretty as Json
import qualified Data.ByteString.Lazy     as BL
import           Data.Foldable            (for_)
import qualified Data.HashMap.Strict      as HM
import qualified Data.IORef               as IOR
import           Data.List                (isSuffixOf, sortOn)
import           Data.Maybe               (fromMaybe, maybeToList)
import qualified Data.Text                as T
import qualified Data.Text.IO             as T
import           Data.Time.Calendar       (fromGregorian)
import           Data.Time.Clock          (UTCTime (UTCTime), addUTCTime,
                                           getCurrentTime)
import           Data.Traversable         (for)
import qualified Data.Yaml                as Yaml
import           Safe                     (maximumDef)
import qualified System.Directory         as Dir
import           System.Envy              (FromEnv, decodeEnv, env, fromEnv)
import           System.Exit              (die)
import           System.FilePath          ((</>))
import           System.IO                (BufferMode (NoBuffering), hGetEcho,
                                           hPrint, hPutStrLn, hSetBuffering,
                                           hSetEcho, stderr, stdin, stdout)
import qualified Web.Slack                as Slack
import qualified Web.Slack.Common         as Slack
import           Web.Slack.Conversation   (ConversationId (ConversationId, unConversationId))
import qualified Web.Slack.Conversation   as Conversation
import qualified Web.Slack.Pager          as Slack
import qualified Web.Slack.User           as User

import           SlackLog.Html
import           SlackLog.Pagination      (chooseLatestPageOf, defaultPageSize,
                                           paginateFiles)
import           SlackLog.Replies
import           SlackLog.Types           (ChannelId, Config (..),
                                           Duration (..), targetChannels)
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

    -- These actions have to be performed before generating HTMLs.
    -- Because generating HTMLs requires channelsByName, usersByName
    (`runReaderT` apiConfig) $ do
      saveUsersList

      let targets = targetChannels config
      saveResult <- for (HM.keys targets) $ \chanId -> do
        newTs <- saveChannel oldTss chanId

        now <- liftIO getCurrentTime
        saveReplies config now $ ConversationId chanId

        liftIO $ do
          jsonPaths <- collectTargetJsons chanId
          convertJsonsInChannel ws chanId jsonPaths
          return ((chanId, newTs), (chanId, jsonPaths))

      liftIO $ do
        let (newTss, newNames) = Arrow.first HM.fromList $ unzip saveResult
        BL.writeFile "json/.timestamps.json" $ Json.encodePretty newTss

        when (oldTss /=  newTss) $
          generateIndexHtml ws newNames


-- |
-- The directory structure of replies: @docs/json/<CHANNEL_ID>/<PAGE_NUM>/<MESSAGE_TIMESTAMP>.json@.
-- Assumes the current directory is @docs/@
saveReplies :: Config -> UTCTime -> ConversationId -> ReaderT Slack.SlackConfig IO ()
saveReplies Config { saveRepliesBefore = Duration before } now convId = do
  apiConfig <- ask
  liftIO . Dir.withCurrentDirectory "json" $ do
    let saveSince = addUTCTime (negate before) now
    putStrLn
      $ "Channel "
      ++ T.unpack (unConversationId convId)
      ++ ": Started `saveReplies` of messages with replies since "
      ++ show saveSince
    threads <- searchThreadsAppendedSince saveSince convId
    for_ threads $ \Thread { tFirstMessage, tLatestTs, tMessages, tPath } -> do
      putStrLn
        $ "Channel "
        ++ T.unpack (unConversationId convId)
        ++ ": Saving a thread "
        ++ tPath
      toAppend <- (`runReaderT` apiConfig) $ do
        let threadId = Slack.messageTs tFirstMessage
            repliesReq = (Conversation.mkRepliesReq convId threadId)
              { Conversation.repliesReqInclusive = False
              , Conversation.repliesReqOldest = Just tLatestTs
              }
        loadPage <- Slack.repliesFetchAll repliesReq
        Slack.loadingPage loadPage $
          either throwM (return . dropThreadMessage threadId)
      appendToThreadFile convId tPath tFirstMessage tMessages toAppend


-- | Assumes the current directory is the project root
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


-- | Assumes the current directory is @docs/@
saveUsersList :: ReaderT Slack.SlackConfig IO ()
saveUsersList = do
  result <- Slack.usersList
  liftIO $
    case result of
      Right (User.ListRsp us) -> do
        let usersByName = HM.fromList $ map ((,) <$> Slack.unUserId . User.userId <*> User.userName) us
        BL.writeFile "json/.users.json" $ Json.encodePretty usersByName
      Left err -> do
        hPutStrLn stderr "WARNING: Error when fetching the list of users:"
        hPrint stderr err


-- | Assumes the current directory is @docs/@
saveChannel
  ::  TimestampsByChannel
  -> ChannelId
  -> ReaderT Slack.SlackConfig IO Slack.SlackTimestamp
saveChannel tss chanId = do
  let old = fromMaybe (Slack.mkSlackTimestamp $ UTCTime (fromGregorian 2017 1 1) 0) (HM.lookup chanId tss)
  liftIO . putStrLn $ "Channel " ++ T.unpack chanId ++ "'s last timestamp: " ++ show (Slack.slackTimestampTime old) ++ "."
  let histReq = (Conversation.mkHistoryReq (ConversationId chanId))
        { Conversation.historyReqInclusive = False
        , Conversation.historyReqOldest = Just old
        }
  latestTsRef <- liftIO $ IOR.newIORef old
  loadPage <- Slack.conversationsHistoryAll histReq
  Slack.loadingPage loadPage $ \epage -> liftIO $
    case epage of
        Right msgs ->
          if null msgs
            then
              putStrLn $ "Channel " ++ T.unpack chanId ++ ": Empty page returned by LoadPage. Finishing to save."
            else do
              lastTs <- IOR.readIORef latestTsRef
              putStrLn $ "Channel " ++ T.unpack chanId ++ ": Saving page since " ++ show (Slack.slackTimestampTime lastTs) ++ "."

              let latestTs = maximumDef lastTs $ map Slack.messageTs msgs
              IOR.writeIORef latestTsRef latestTs

              addMessagesToChannelDirectory chanId $ sortOn Slack.messageTs msgs
        Left err -> do
          hPutStrLn stderr $ "WARNING: Error when fetching the history of " ++ show chanId ++ ":"
          hPrint stderr err
  liftIO $ do
    putStrLn $ "Channel " ++ T.unpack chanId ++ ": Finishing saving the channel history."
    IOR.readIORef latestTsRef


-- | Assumes the current directory is @docs/@
addMessagesToChannelDirectory :: ChannelId -> [Slack.Message] -> IO ()
addMessagesToChannelDirectory chanId msgs =
  Dir.withCurrentDirectory "json" $ do
    let channelNameS = T.unpack chanId
        tmpFileName = channelNameS <> "-tmp.json"
    BL.writeFile tmpFileName $ Json.encodePretty msgs
    Dir.createDirectoryIfMissing False channelNameS

    -- NOTE: Duplicate logic with `collectTargetJsons`. But I'm not sure how to refactor.
    channelDirItems <- filter (isSuffixOf ".json") <$> Dir.listDirectory channelNameS
    (mLatestPageFileName, basePageNum) <-
      if null channelDirItems
        then return (Nothing, 1)
        else Arrow.first (Just . (channelNameS </>)) <$> chooseLatestPageOf channelDirItems
    putStrLn $ "Paginating files from " ++ show mLatestPageFileName
    -- OPTIMIZE: Return updated/created JSON files so that `convertJsonsInChannel` can convert only them.
    --           Current implementation of `convertJsonsInChannel` converts *all* JSON files anytime when
    --           messages are fetched.
    paginateFiles defaultPageSize basePageNum channelNameS (maybeToList mLatestPageFileName ++ [tmpFileName])
    Dir.removeFile tmpFileName
