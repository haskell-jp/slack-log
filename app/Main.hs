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
import qualified Data.Aeson               as Json
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
import qualified Web.Slack.User           as User

import           SlackLog.Pagination      (chooseLatestPageOf, defaultPageSize,
                                           paginateFiles)
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
  , "CCYF8H43A" -- nix
  , "CD87P78HF" -- mmlh
  , "CE368SB5G" -- ghc8x
  ]


type TimestampsByChannel = HM.HashMap T.Text Slack.SlackTimestamp


main :: IO ()
main = do
  hSetBuffering stdin  NoBuffering
  hSetBuffering stdout NoBuffering
  hSetBuffering stderr NoBuffering

  config <- Slack.mkSlackConfig =<< slackApiToken <$> (failWhenLeft =<< decodeEnv)
  tss <- readLastTimestampsOrDefault ".timestamps.json"
  newTss <- HM.fromList <$> for targetChannels (saveChannel config tss)

  BL.writeFile ".timestamps.json" $ Json.encodePretty newTss

  Slack.channelsList (Channel.ListReq (Just True) (Just False))
    `runReaderT` config >>= \case
      Right (Channel.ListRsp chs) -> do
        let channelsByName = HM.fromList $ map ((,) <$> Channel.channelId <*> Channel.channelName) chs
        BL.writeFile "doc/json/.channels.json" $ Json.encodePretty channelsByName
      Left err -> do
        hPutStrLn stderr "WARNING: Error when fetching the list of channels:"
        hPrint stderr err

  Slack.usersList
    `runReaderT` config >>= \case
      Right (User.ListRsp us) -> do
        let usersByName = HM.fromList $ map ((,) <$> Slack.unUserId . User.userId <*> User.userName) us
        BL.writeFile "doc/json/.users.json" $ Json.encodePretty usersByName
      Left err -> do
        hPutStrLn stderr "WARNING: Error when fetching the list of users:"
        hPrint stderr err

  when (tss /= newTss) gitPushMessageLog


readLastTimestampsOrDefault :: FilePath -> IO TimestampsByChannel
readLastTimestampsOrDefault path = do
  result <- Json.eitherDecode <$> BL.readFile path
  case result of
      Right tbc -> return tbc
      Left err  -> fail $ "Error reading \"" ++ path ++ "\"\n" ++ show err


saveChannel :: Slack.SlackConfig -> TimestampsByChannel -> T.Text -> IO (T.Text, Slack.SlackTimestamp)
saveChannel cfg tss channelName = Dir.withCurrentDirectory "doc/json" $ do
  new <- Slack.mkSlackTimestamp <$> getCurrentTime
  let old = fromMaybe (Slack.mkSlackTimestamp $ UTCTime (fromGregorian 2017 1 1) 0) (HM.lookup channelName tss)
  print old
  res <- runReaderT (Slack.historyFetchAll Slack.channelsHistory channelName 100 old new) cfg
  case res of
      Right body -> do
        let msgs = Slack.historyRspMessages body
            mbLatestTs = Slack.messageTs <$> headMay msgs
        case mbLatestTs of
            Just latestTs -> do
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
              return (channelName, latestTs)
            _ -> do
              hPutStrLn stderr $ "WARNING: Error when fetching the history of " ++ show channelName ++ ": " ++ "Can't get latest timestamp!"
              return (channelName, old)
      Left err -> do
        hPutStrLn stderr $ "WARNING: Error when fetching the history of " ++ show channelName ++ ":"
        hPrint stderr err
        return (channelName, old)


failWhenLeft :: Either String a -> IO a
failWhenLeft = either fail return


gitPushMessageLog :: IO ()
gitPushMessageLog = do
  P.runProcess_ $ P.proc "git" ["add", "./.timestamps.json", "./doc/"]
  now <- getCurrentTime
  P.runProcess_ $ P.proc "git" ["commit", "-m", "Slack log update at " ++ show now]
  P.runProcess_ $ P.proc "git" ["push"]
