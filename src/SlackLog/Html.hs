{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE StrictData        #-}

module SlackLog.Html
  ( convertToHtmlFile
  , renderSlackMessages
  , loadWorkspaceInfo
  , PageInfo(..)
  , WorkspaceInfo(..)
  ) where


import qualified Data.Aeson              as Json
import qualified Data.ByteString.Lazy    as BL
import qualified Data.HashMap.Strict     as HM
import qualified Data.Text               as T
import qualified Data.Time.Clock         as TC
import qualified Data.Time.Format        as TF
import qualified Data.Time.LocalTime     as LT
import qualified Data.Time.Zones         as TZ
import           Html                    (( # ))
import qualified Html                    as H
import qualified Html.Attribute          as A
import           System.FilePath         ((</>))
import qualified Web.Slack.Common        as Slack
import qualified Web.Slack.MessageParser as Slack

import           SlackLog.Types          (ChannelId, ChannelName,
                                          Config (timeZone, workspaceName),
                                          UserId, UserName)
import           SlackLog.Util           (failWhenLeft)


data PageInfo = PageInfo
  { pageNumber       :: Integer
  , previousPagePath :: Maybe FilePath
  , nextPagePath     :: Maybe FilePath
  , channelId        :: ChannelId
  } deriving (Eq, Show)

data WorkspaceInfo = WorkspaceInfo
  { userNameById      :: HM.HashMap UserId UserName
  , channelNameById   :: HM.HashMap ChannelId ChannelName
  , groupNameById     :: HM.HashMap ChannelId ChannelName
  , workspaceInfoName :: T.Text
  , getTimeDiff       :: TC.UTCTime -> LT.TimeZone
  }


convertToHtmlFile :: WorkspaceInfo -> PageInfo -> IO ()
convertToHtmlFile ws pg = do
  let jsonPath = pathFromPageInfo "json" pg
      htmlPath = pathFromPageInfo "html" pg
  BL.writeFile htmlPath
    =<< fmap (renderSlackMessages ws pg)
      . failWhenLeft
    =<< Json.eitherDecodeFileStrict' jsonPath


-- | Assumes this function is executed in doc/ directory
--   So the returned path of JSON and HTML should be prefixed with "json/" or "html/"
pathFromPageInfo :: FilePath -> PageInfo -> FilePath
pathFromPageInfo dirName PageInfo { channelId, pageNumber } =
  dirName </> T.unpack channelId </> show pageNumber ++ "." ++ dirName


renderSlackMessages :: WorkspaceInfo -> PageInfo -> [Slack.Message] -> BL.ByteString
renderSlackMessages WorkspaceInfo {..} PageInfo {..} msgs = H.renderByteString
  ( H.doctype_
  # H.html_
    ( H.head_
      ( H.meta_A (A.charset_ ("utf-8" :: T.Text))
      # H.title_ title
      # H.link_A
        ( A.rel_ ("stylesheet" :: T.Text)
        # A.href_ ("style.css" :: T.Text)
        # A.type_ ("text/css" :: T.Text)
        # A.media_ ("screen" :: T.Text)
        )
      )
    # H.body_
      ( H.h1_ title
      # pager
      # H.div_A (A.class_ ("message_list" :: T.Text)) (map messageDiv msgs)
      # pager
      )
    )
  )
 where
  channelName = channelNameById HM.! channelId
  title = workspaceInfoName <> " / " <> channelName <> " #" <> T.pack (show pageNumber)
  pager = H.div_A (A.class_ ("pager" :: T.Text))
    ( ((\pp -> H.a_A (A.href_ pp # A.class_ ("pager__previous" :: T.Text)) ("Previous" :: T.Text)) <$> previousPagePath)
    # ((\pp -> H.a_A (A.href_ pp # A.class_ ("pager__next" :: T.Text)    ) ("Next"     :: T.Text)) <$> nextPagePath)
    )
  messageDiv Slack.Message {messageTs, messageUser, messageText} =
    H.div_A (A.class_ ("message" :: T.Text) # A.id_ ("message-" <> Slack.slackTimestampTs messageTs))
      ( H.div_A (A.class_ ("message__timestamp" :: T.Text))
        (H.Raw . timestampBlock $ Slack.slackTimestampTime messageTs)
      # H.div_A (A.class_ ("message__header" :: T.Text))
        userName
      # H.div_A (A.class_ ("message__body" :: T.Text))
        (H.Raw messageBody)
      )
   where
    getUserName suid =
      let uid = Slack.unUserId suid in fromMaybe uid (HM.lookup uid userNameById)
    userName = maybe "<non-user>" getUserName messageUser
    messageBody =
      Slack.messageToHtml Slack.defaultHtmlRenderers getUserName messageText
    timestampBlock tm =
      let lt = LT.utcToZonedTime (getTimeDiff tm) tm
      in TF.formatTime TF.defaultTimeLocale "%Y-%m-%d<br/>%T %z" lt


-- | Assumes this function is executed in doc/ directory
loadWorkspaceInfo :: FilePath -> IO WorkspaceInfo
loadWorkspaceInfo dir = do
  userNameById <- failWhenLeft =<< Json.eitherDecodeFileStrict' (dir </> ".users.json")
  channelNameById <- failWhenLeft =<< Json.eitherDecodeFileStrict' (dir </> ".channels.json")
  groupNameById <- failWhenLeft =<< Json.eitherDecodeFileStrict' (dir </> ".groups.json")

  cfg <- failWhenLeft =<< Json.eitherDecodeFileStrict' (dir </> ".config.json")
  let workspaceInfoName = workspaceName cfg
  getTimeDiff <- fmap TZ.timeZoneForUTCTime . TZ.loadTZFromDB $ timeZone cfg

  return WorkspaceInfo {..}
