module SlackLog.Util where

import qualified Data.Aeson as Json


failWhenLeft :: Either String a -> IO a
failWhenLeft = either fail return


readJsonFile :: Json.FromJSON a => FilePath -> IO a
readJsonFile path = do
  result <- Json.eitherDecodeFileStrict path
  case result of
      Right tbc -> return tbc
      Left err  -> fail $ "Error reading \"" ++ path ++ "\"\n" ++ show err
