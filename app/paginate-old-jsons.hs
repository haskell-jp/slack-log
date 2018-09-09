import           Control.Monad (filterM)
import           Data.Char           (isAlphaNum)
import           Data.Foldable       (for_)
import           Data.List           (groupBy, isPrefixOf, sortBy)
import           Data.Monoid         ((<>))
import           Data.Ord            (comparing)
import qualified System.Directory    as Dir

import           Data.NamedPage

main :: IO ()
main = do
  Dir.setCurrentDirectory "doc/json"
  channelLogPaths <- filterM isMessageLogJson =<< Dir.listDirectory "."
  let logPathsByChannel =
        groupBy (\x -> (== EQ) . comparing extractChannelName x)
          $ sortBy existingMessageLogsOrder channelLogPaths
  for_ logPathsByChannel $ \sameChannelPaths -> do
    let channelName = extractChannelName $ head sameChannelPaths

    paginateFiles defaultPageSize channelName sameChannelPaths
    -- putStrLn channelName
    -- mapM_ (putStrLn . ("  " ++)) sameChannelPaths

extractChannelName :: FilePath -> String
extractChannelName = takeWhile isAlphaNum


isMessageLogJson :: FilePath -> IO Bool
isMessageLogJson path = -- isPrefixOf "C4LFB6DE0"
  (&&) (not $ isPrefixOf "." path) <$> (not <$> Dir.doesDirectoryExist path)


existingMessageLogsOrder :: FilePath -> FilePath -> Ordering
existingMessageLogsOrder a b =
  comparing extractChannelName a b <> comparing length a b <> compare a b
