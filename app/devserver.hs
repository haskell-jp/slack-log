{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import System.FilePath ((</>))

import qualified System.Environment       as System

import qualified Network.Wai              as Http
import qualified Network.Wai.Handler.Warp as Http
import qualified Network.HTTP.Types       as Http

import qualified Data.ByteString          as BS
import qualified Data.ByteString.UTF8     as BS


main :: IO ()
main = do
  config <- getAppConfig
  runServer config


data AppConfig = AppConfig
  { serverPort :: Int
  , targetDir :: String
  }
  deriving (Eq, Show)

getAppConfig :: IO AppConfig
getAppConfig = do
  args <- System.getArgs

  let port = case args of
        []  -> 5050
        a:_ -> read a

  pure $ AppConfig
    { serverPort = port
    , targetDir = "docs"
    }


runServer :: AppConfig -> IO ()
runServer AppConfig{..} = Http.run serverPort server
  where
    server request respond = respond $ case Http.rawPathInfo request of
      "/" -> Http.responseLBS
        Http.status301
        [
          ("Content-Type", "text/plain"),
          ("Location", "/slack-log/index.html")
        ]
        "Redirecting..."

      rawPath | Just path <- BS.stripPrefix "/slack-log/" rawPath -> Http.responseFile
        Http.status200
        [("Content-Type", "text/html")]
        (targetDir </> BS.toString path)
        Nothing

      _ -> Http.responseLBS
          Http.notFound404
          [("Content-Type", "text/plain")]
          "404 Not Found"
