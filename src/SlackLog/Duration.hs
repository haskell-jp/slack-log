{-# LANGUAGE OverloadedStrings #-}

module SlackLog.Duration where

import           Control.Monad   (when)
import qualified Data.Aeson      as Json
import qualified Data.Text.Read  as TR
import qualified Data.Time.Clock as C


newtype Duration = Duration { asNominalDiffTime :: C.NominalDiffTime }
  deriving (Eq, Show)

instance Json.FromJSON Duration where
  parseJSON = Json.withText "SlackLog.Duration" $ \t -> either fail pure $ do
    (i, left) <- TR.decimal t
    when (i < 1) $ Left "duration must be positive"
    scale <-
      case left of
        "m"   -> pure 60
        "h"   -> pure $ 60 * 60
        "d"   -> pure $ 60 * 60 * 24
        "w"   -> pure $ 60 * 60 * 24 * 7
        other -> Left $ "Unknown scale: " ++ show other
    pure . Duration $ fromInteger i * scale
