module SlackLog.Replies
  ( searchThreadsAppendedSince
  , dropThreadMessage
  , appendToThreadFile
  , Thread (..)
  ) where

import qualified Data.Aeson.Encode.Pretty as Json
import qualified Data.ByteString.Lazy     as BL
import           Data.List                (isSuffixOf, sortOn)
import qualified Data.Text                as T
import           Data.Time.Clock          (UTCTime)
import qualified Data.Yaml                as Yaml
import           Safe                     (lastNote)
import qualified System.Directory         as Dir
import           System.FilePath          ((</>))
import qualified System.FilePath          as FP
import qualified Web.Slack.Common         as Slack
import           Web.Slack.Conversation   (ConversationId (unConversationId))


appendToThreadFile :: FilePath -> Slack.Message -> [Slack.Message] -> [Slack.Message] -> IO ()
appendToThreadFile threadFilePath firstMessage existingMessages fetchedMessages =
  BL.writeFile threadFilePath $ Json.encodePretty messages
 where
  messages =
    firstMessage : existingMessages ++ sortOn Slack.messageTs fetchedMessages
{-# INLINE appendToThreadFile #-}


dropThreadMessage :: Slack.SlackTimestamp -> [Slack.Message] -> [Slack.Message]
dropThreadMessage threadId = filter ((/= threadId) . Slack.messageTs)
{-# INLINE dropThreadMessage #-}


data Thread = Thread
  { tFirstMessage :: !Slack.Message
  , tLatestTs     :: !Slack.SlackTimestamp
  , tMessages     :: ![Slack.Message]
  , tPath         :: FilePath
  } deriving (Eq, Show)


-- | Assumes the current directory is `docs/json/`
searchThreadsAppendedSince :: UTCTime -> ConversationId -> IO [Thread]
searchThreadsAppendedSince saveSince convId = do
  messagesJsons <- Dir.listDirectory chanDir
  Dir.withCurrentDirectory chanDir .
    concatMapM f $ filter (isSuffixOf ".json") messagesJsons
 where
  f :: FilePath -> IO [Thread]
  f messagesJson =
    mapMaybeM (f' messagesJson) =<< Yaml.decodeFileThrow messagesJson

  f' :: FilePath -> Slack.Message -> IO (Maybe Thread)
  f' messagesJson firstMessage = do
    let threadsDir = FP.dropExtension messagesJson
    getThreadSummaryIfRepliedSince saveSince threadsDir firstMessage

  chanDir = T.unpack $ unConversationId convId


-- | Assumes the current directory is @docs/json/CHANNEL_ID/@.
--   Check the timestamp of the last message in @PAGE_NUM/MESSAGE_TIMESTAMP.json@.
getThreadSummaryIfRepliedSince :: UTCTime -> FilePath -> Slack.Message -> IO (Maybe Thread)
getThreadSummaryIfRepliedSince saveSince threadsDir firstMessage = do
  let firstMessageTs = Slack.messageTs firstMessage
      threadFilePath = threadsDir </> FP.addExtension (T.unpack (Slack.slackTimestampTs firstMessageTs)) "json"
  hasThreadFile <- Dir.doesPathExist threadFilePath
  if hasThreadFile
    then do
      messages <- Yaml.decodeFileThrow threadFilePath
      let e = "Assertion failed: " ++ threadFilePath ++ " doesn't contain any messages!"
          lastMessageTs = Slack.messageTs $ lastNote e (messages :: [Slack.Message])
      if Slack.slackTimestampTime lastMessageTs > saveSince
        then
          return $ Just Thread
            { tFirstMessage = firstMessage
            , tLatestTs     = lastMessageTs
            , tMessages     = dropThreadMessage firstMessageTs messages
            , tPath         = threadFilePath
            }
        else return Nothing
    else
      if Slack.slackTimestampTime firstMessageTs > saveSince
        then
          return $ Just Thread
            { tFirstMessage = firstMessage
            , tLatestTs     = firstMessageTs
            , tMessages     = []
            , tPath         = threadFilePath
            }
        else return Nothing


-- | A version of 'concatMap' that works with a monadic predicate.
--   Ref. https://www.stackage.org/haddock/lts-16.28/extra-1.7.9/src/Control.Monad.Extra.html#concatMapM
concatMapM :: Monad m => (a -> m [b]) -> [a] -> m [b]
concatMapM op = foldr f (pure [])
 where
  f x xs = do
    x' <- op x
    if null x'
      then xs
      else do
        xs' <- xs
        pure $ x' ++ xs'
{-# INLINE concatMapM #-}


-- | A version of 'mapMaybe' that works with a monadic predicate.
--   Ref. https://www.stackage.org/haddock/lts-16.29/extra-1.7.9/src/Control.Monad.Extra.html#mapMaybeM
mapMaybeM :: Monad m => (a -> m (Maybe b)) -> [a] -> m [b]
{-# INLINE mapMaybeM #-}
mapMaybeM op = foldr f (pure [])
 where
  f x xs = do
    mx' <- op x
    case mx' of
        Nothing -> xs
        Just x' -> do
          xs' <- xs
          pure $ x' : xs'
