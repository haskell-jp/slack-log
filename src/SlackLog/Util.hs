module SlackLog.Util where

failWhenLeft :: Either String a -> IO a
failWhenLeft = either fail return
