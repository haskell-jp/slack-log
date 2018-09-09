{-# LANGUAGE TypeApplications #-}

import           Data.Char           (isAlphaNum)
import           Data.Foldable       (for_)
import           Data.List           (groupBy, isPrefixOf, sortBy)
import           Data.Monoid         ((<>))
import           Data.Ord            (comparing)
import qualified System.Directory    as Dir
import qualified Web.Slack.Common as Slack

import           Data.NamedPage
import           Web.Slack.Instances ()

main :: IO ()
main = do
  Dir.setCurrentDirectory "doc/json"
  channelLogPaths <- filter isMessageLogJson <$> Dir.listDirectory "."
  let logPathsByChannel =
        groupBy (\x -> (== EQ) . comparing extractChannelName x)
          $ sortBy existingMessageLogsOrder channelLogPaths
  for_ logPathsByChannel $ \sameChannelPaths -> do
    let channelName = extractChannelName $ head sameChannelPaths
        files       = map mkFileObj sameChannelPaths

    writeNamedPages =<< repaginateJsons @IO @Slack.Message defaultPageSize channelName files
    -- putStrLn channelName
    -- mapM_ (putStrLn . ("  " ++) . filePath) files

extractChannelName :: FilePath -> String
extractChannelName = takeWhile isAlphaNum


isMessageLogJson :: FilePath -> Bool
isMessageLogJson = -- isPrefixOf "C4LFB6DE0"
  not . isPrefixOf "."


existingMessageLogsOrder :: FilePath -> FilePath -> Ordering
existingMessageLogsOrder a b =
  comparing extractChannelName a b <> comparing length a b <> compare a b
