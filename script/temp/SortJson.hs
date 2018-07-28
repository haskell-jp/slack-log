#!/usr/bin/env stack
-- stack script --resolver lts-11.14

{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE LambdaCase       #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TypeOperators    #-}

import           Control.Lens             (view, (^.))
import qualified Data.Aeson               as Json
import qualified Data.Aeson.Encode.Pretty as Json
import qualified Data.ByteString.Lazy     as BL
import           Data.Extensible
import           Data.List                (sortBy)
import           Data.Maybe               (listToMaybe)
import           Data.Ord                 (comparing)
import           Data.Text                (Text)
import           System.Environment       (getArgs)

main :: IO ()
main = mapM_ (sortJsonBy (comparing $ view #ts)) =<< getArgs


type Message1 = Record '[ "historyRspMessages" >: [Message2] ]

type Message2 = Record
   '[ "messageType" >: Text
    , "messageText" >: (Record '["unSlackMessageText" >: Text])
    , "messageTs"   >: Text
    , "messageUser" >: Maybe (Record '["unUserId" >: Text])
    ]

type Message3 = Record
   '[ "ts"   >: Text
    , "text" >: Text
    , "user" >: Maybe Text
    , "type" >: Text
    ]

sortJsonBy :: (Message3 -> Message3 -> Ordering) -> FilePath -> IO ()
sortJsonBy f path = do
  bs <- BL.readFile path
  json <- case Json.eitherDecode bs of
      Right msg3 -> return msg3
      _ -> case Json.eitherDecode bs of
          Right msg2 -> return $ fromMessage2 msg2
          _ -> case Json.eitherDecode bs of
              Right msg1 -> return $ fromMessage1 msg1
              Left err   -> fail $ show err
  BL.writeFile path (Json.encodePretty $ sortBy f json)


fromMessage2 :: [Message2] -> [Message3]
fromMessage2 = map $ \msg ->
      #ts   @= (msg ^. #messageTs)
   <: #text @= (msg ^. #messageText ^. #unSlackMessageText)
   <: #user @= (view #unUserId <$> msg ^. #messageUser)
   <: #type @= (msg ^. #messageType)
   <: nil

fromMessage1 :: Message1 -> [Message3]
fromMessage1 = fromMessage2 . view #historyRspMessages
