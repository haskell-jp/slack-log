{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

module SlackLog.Pagination
  ( NamedPage(..)
  , Name
  , FileObj(..)
  , mkFileObj
  , paginateFiles
  , chooseLatestPageOf
  , repaginate
  , repaginateJsons
  , writeNamedPages
  , defaultPageSize
  )
where


import           Control.Monad.Fail       (fail)
import           Data.Aeson               (FromJSON, ToJSON, eitherDecode)
import           Data.Aeson.Encode.Pretty (encodePretty)
import qualified Data.ByteString          as B
import qualified Data.ByteString.Lazy     as BL
import           Data.Char                (isDigit)
import           Data.Foldable            (maximumBy)
import           Data.List.Extra          (chunksOf)
import           Data.Ord                 (comparing)
import qualified System.Directory         as Dir
import           System.FilePath          (addExtension, takeDirectory)
import qualified Test.QuickCheck          as QC
import           Text.Read                (readMaybe)
import qualified Web.Slack.Common         as Slack

import           Web.Slack.Instances      ()

import           Prelude                  hiding (fail)


type Name = String


data NamedPage a = NamedPage
  { namedPageName :: !Name
  , namedPagePage :: ![a]
  } deriving (Eq, Show)

instance QC.Arbitrary a => QC.Arbitrary (NamedPage a) where
  arbitrary = NamedPage <$> QC.arbitrary <*> (QC.getNonEmpty <$> QC.arbitrary)


data FileObj m = FileObj
  { filePath    :: !FilePath
  , readFileObj :: !(m BL.ByteString)
  }


mkFileObj :: FilePath -> FileObj IO
mkFileObj path = FileObj path (BL.fromStrict <$> B.readFile path)
--                            ^ Read all data strictly to safely overwrite files


paginateFiles :: Int -> Integer -> Name -> [FilePath] -> IO ()
paginateFiles size basePageNum baseName paths =
  writeNamedPages =<< repaginateJsons @IO @Slack.Message size
                                                         basePageNum
                                                         baseName
                                                         (map mkFileObj paths)


chooseLatestPageOf
  :: forall m . MonadFail m => [FilePath] -> m (FilePath, Integer)
chooseLatestPageOf = fmap (maximumBy (comparing snd)) . mapM f
 where
  f path = case readMaybe $ takeWhile isDigit path of
    Just pageN -> return (path, pageN)
    _          -> fail $ "Assertion failure: Invalid path " ++ show path


repaginate
  :: forall a . Int -> Integer -> Name -> [NamedPage a] -> [NamedPage a]
repaginate n basePageNum baseName =
  zipWith toNamedPage [basePageNum ..] . chunksOf n . concatMap namedPagePage
 where
  toNamedPage :: Integer -> [a] -> NamedPage a
  toNamedPage pn = NamedPage (baseName ++ "/" ++ show pn)


repaginateJsons
  :: forall m a
   . (MonadFail m, FromJSON a)
  => Int
  -> Integer
  -> Name
  -> [FileObj m]
  -> m [NamedPage a]
repaginateJsons n basePageNum baseName =
  fmap (repaginate n basePageNum baseName)
    . mapM (\f -> NamedPage (filePath f) <$> toJson f)
 where
  toJson :: FileObj m -> m [a]
  toJson f = either fail return . eitherDecode =<< readFileObj f


writeNamedPages :: ToJSON a => [NamedPage a] -> IO ()
writeNamedPages = mapM_ $ \np -> do
  let name = namedPageName np
  Dir.createDirectoryIfMissing False $ takeDirectory name
  BL.writeFile (addExtension name ".json") $ encodePretty $ namedPagePage np


defaultPageSize :: Int
defaultPageSize = 50
