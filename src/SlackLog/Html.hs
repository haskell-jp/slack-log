{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE StrictData        #-}

-- | Assumes functions in this module are executed in doc/ directory

module SlackLog.Html
  ( convertJsonsInChannel
  , generateIndexHtml
  , collectTargetJsons
  , renderSlackMessages
  , renderIndexOfPages
  , loadWorkspaceInfo
  , parsePageNumber
  , PageInfo(..)
  , WorkspaceInfo(..)
  ) where


import           Control.Applicative     ((<|>))
import           Control.Monad           ((<=<))
import qualified Data.Aeson              as Json
import qualified Data.ByteString.Lazy    as BL
import           Data.Char               (isDigit)
import           Data.Foldable           (for_)
import qualified Data.HashMap.Strict     as HM
import           Data.List               (sortOn)
import           Data.Maybe              (fromMaybe)
import qualified Data.Text               as T
import qualified Data.Time.Clock         as TC
import qualified Data.Time.Format        as TF
import qualified Data.Time.LocalTime     as LT
import qualified Data.Time.Zones         as TZ
import           Html                    (( # ))
import qualified Html                    as H
import qualified Html.Attribute          as A
import           Safe                    (lastMay)
import qualified System.Directory        as Dir
import           System.FilePath         (takeBaseName, (<.>), (</>))
import qualified Web.Slack.Common        as Slack
import qualified Web.Slack.MessageParser as Slack

import           SlackLog.Types          (ChannelId, ChannelName, Config (..),
                                          UserId, UserName, TargetChannel(..))
import           SlackLog.Util           (failWhenLeft, readJsonFile)


data PageInfo = PageInfo
  { currentPagePath  :: FilePath
  , previousPagePath :: Maybe FilePath
  , nextPagePath     :: Maybe FilePath
  , channelId        :: ChannelId
  } deriving (Eq, Show)

data WorkspaceInfo = WorkspaceInfo
  { userNameById          :: HM.HashMap UserId UserName
  , channelNameById       :: HM.HashMap ChannelId ChannelName
  , groupNameById         :: HM.HashMap ChannelId ChannelName
  , workspaceInfoName     :: T.Text
  , getTimeDiff           :: TC.UTCTime -> LT.TimeZone
  }


collectTargetJsons :: ChannelId -> IO [FilePath]
collectTargetJsons = Dir.listDirectory . ("json" </>) . T.unpack


convertJsonsInChannel :: WorkspaceInfo -> ChannelId -> [FilePath] -> IO ()
convertJsonsInChannel ws chanId jsonPaths = do
  let channelIdStr = T.unpack chanId

  Dir.createDirectoryIfMissing True $ "html" </> channelIdStr

  putStrLn channelIdStr

  let triples =
        putBetweenPreviousAndNext $ sortOn parsePageNumber jsonPaths

  for_ triples $ \(mPrev, name, mNext) -> do
    let pg = PageInfo name mPrev mNext chanId
    putStrLn $ "  " ++ show pg
    convertToHtmlFile ws pg


convertToHtmlFile :: WorkspaceInfo -> PageInfo -> IO ()
convertToHtmlFile ws pg =
  BL.writeFile htmlPath =<< renderSlackMessages ws pg
 where
  cid = channelId pg
  currentPath = currentPagePath pg
  htmlPath = ensurePathIn "html" cid currentPath


generateIndexHtml :: WorkspaceInfo -> [(ChannelId, [FilePath])] -> IO ()
generateIndexHtml ws = BL.writeFile "index.html" <=< renderIndexOfPages ws


renderSlackMessages :: WorkspaceInfo -> PageInfo -> IO BL.ByteString
renderSlackMessages wsi@WorkspaceInfo {..} PageInfo {..} =
  fmap render . readJsonFile $ ensurePathIn "json" channelId currentPagePath
 where
  render msgs = H.renderByteString
    ( H.doctype_
    # H.html_
      ( H.head_
        ( H.meta_A (A.charset_ "utf-8")
        # H.title_ title
        # H.link_A
          ( A.rel_ "stylesheet"
          # A.href_ "../../main.css"
          # A.type_ "text/css"
          # A.media_ "screen"
          )
        )
      # H.body_
        ( H.div_A (A.class_ "ui container")
          ( H.h1_ title
          # pager
          # H.div_A (A.class_ "message_list ui feed") (map messageDiv msgs)
          # pager
          )
        )
      )
    )

  title =
    (  workspaceInfoName
    <> T.pack " / " <> getChannelScreenName wsi channelId
    <> T.pack " #" <> T.pack (show (parsePageNumber currentPagePath))
    )

  pager = H.div_A (A.class_ "pager ui pagination menu")
    ( ((\pp -> H.a_A (A.href_ ("../../" ++ pp) # prevClass) prevLabel) . ensurePathIn "html" channelId <$> previousPagePath)
    #          H.a_A (A.href_ "../../" # topClass ) topLabel
    # ((\pp -> H.a_A (A.href_ ("../../" ++ pp) # nextClass) nextLabel) . ensurePathIn "html" channelId <$> nextPagePath)
    )
   where
    topClass = A.class_ "pager__top item"
    prevClass = A.class_ "pager__previous item"
    nextClass = A.class_ "pager__next item"

    topLabel = "Top"
    prevLabel = "Previous"
    nextLabel = "Next"

  messageDiv Slack.Message { messageTs, messageUser, messageText } =
    H.div_A (A.class_ "message event" # A.id_ (T.pack "message-" <> Slack.slackTimestampTs messageTs))
      ( H.div_A (A.class_ "label")
        ( H.i_A (A.class_ "user outline icon") ()
        )
      # H.div_A (A.class_ "content")
        ( H.div_A (A.class_ "summary")
          ( H.div_A (A.class_ "message__header user")
            userName
          # H.div_A (A.class_ "message__timestamp date")
            (H.Raw . timestampBlock $ Slack.slackTimestampTime messageTs)
          )
        # H.div_A (A.class_ "message__body description")
          (H.Raw $ mkMessageBody wsi messageText)
        )
      )
   where
    userName = getUserScreenName wsi messageUser
    timestampBlock tm =
      let lt = LT.utcToZonedTime (getTimeDiff tm) tm
      in TF.formatTime TF.defaultTimeLocale "%Y-%m-%d&nbsp;%T %z" lt


loadWorkspaceInfo :: Config -> FilePath -> IO WorkspaceInfo
loadWorkspaceInfo cfg dir = do
  userNameById <- failWhenLeft =<< Json.eitherDecodeFileStrict' (dir </> ".users.json")
  let channelNameById = label <$> targetChannels cfg
  groupNameById <- failWhenLeft =<< Json.eitherDecodeFileStrict' (dir </> ".groups.json")

  let workspaceInfoName = workspaceName cfg
  getTimeDiff <- fmap TZ.timeZoneForUTCTime . TZ.loadTZFromDB $ timeZone cfg

  return WorkspaceInfo {..}


renderIndexOfPages :: WorkspaceInfo -> [(ChannelId, [FilePath])] -> IO BL.ByteString
renderIndexOfPages wsi@WorkspaceInfo {..} =
  fmap wrapBody
    . traverse (\(cid, jsonPaths) -> do
      let sortedJsonPaths = sortOn parsePageNumber jsonPaths
      case lastMay sortedJsonPaths of
          Just lastPath -> do
            lastLastMessage <- readLastMessage $ ensurePathIn "json" cid lastPath
            Just . channelSummary cid lastPath lastLastMessage
              <$> mapM (\path -> channelDetail cid path <$> readFirstMessage (ensurePathIn "json" cid path)) sortedJsonPaths
          _ ->
            return Nothing
      )
 where
  wrapBody body =
    H.renderByteString
      ( H.doctype_
      # H.html_
        ( H.head_
          ( H.meta_A (A.charset_ "utf-8")
          # H.title_ title
          # H.link_A
            ( A.rel_ "stylesheet"
            # A.href_ "main.css"
            # A.type_ "text/css"
            # A.media_ "screen"
            )
          )
        # H.body_
          ( H.div_A (A.class_ "ui container")
            ( H.h1_ title
            # H.div_A (A.class_ "channels_list ui relaxed divided list") body
            )
          )
        )
      )
  title = T.pack "Slack log of " <> workspaceInfoName

  channelSummary cid lastJsonPath Slack.Message { messageTs } details =
    H.div_A (A.class_ "item")
      ( H.div_A (A.class_ "content")
        ( H.details_A (A.class_ "channel")
          ( H.summary_A (A.class_ "channel__name")
            ( H.a_A (A.href_ (ensurePathIn "html" cid lastJsonPath)) (getChannelScreenName wsi cid)
            # " "
            # H.span_A () ("(Last updated at " <> timestampWords (Slack.slackTimestampTime messageTs) <> ")")
            )
          # H.div_A (A.class_ "pages_list ui items") details
          )
        )
      )

  channelDetail cid jsonPath Slack.Message { messageTs, messageUser, messageText } =
    H.div_A (A.class_ "page item")
      ( H.div_A (A.class_ "content")
        ( H.a_A (A.class_ "header" # A.href_ (ensurePathIn "html" cid jsonPath))
          (T.pack "#" <> T.pack (show (parsePageNumber jsonPath)))
        # H.div_A (A.class_ "meta")
          ( H.span_A (A.class_ "page__first_message__header")
            (getUserScreenName wsi messageUser)
          # H.span_A (A.class_ "page__first_message__timestamp")
            (timestampWords $ Slack.slackTimestampTime messageTs)
          )
        # H.div_A (A.class_ "page__first_message__body description")
          (H.Raw $ mkMessageBody wsi messageText)
        )
      )

  readFirstMessage :: FilePath -> IO Slack.Message
  readFirstMessage = fmap head . readJsonFile

  readLastMessage :: FilePath -> IO Slack.Message
  readLastMessage = fmap last . readJsonFile

  timestampWords tm =
    let lt = LT.utcToZonedTime (getTimeDiff tm) tm
    in TF.formatTime TF.defaultTimeLocale "%Y-%m-%d %T %z" lt


mkMessageBody :: WorkspaceInfo -> Slack.SlackMessageText -> T.Text
mkMessageBody =
  Slack.messageToHtml Slack.defaultHtmlRenderers . getUserName


ensurePathIn :: String -> ChannelId -> FilePath -> FilePath
ensurePathIn typ cid name = typ ++ "/" ++ T.unpack cid ++ "/" ++ takeBaseName name <.> typ


getChannelScreenName :: WorkspaceInfo -> ChannelId -> ChannelName
getChannelScreenName WorkspaceInfo {..} cid  =
  fromMaybe cid (HM.lookup cid channelNameById <|> HM.lookup cid groupNameById)


getUserName :: WorkspaceInfo -> Slack.UserId -> UserName
getUserName WorkspaceInfo {..} suid =
  let uid = Slack.unUserId suid in fromMaybe uid (HM.lookup uid userNameById)


getUserScreenName :: WorkspaceInfo -> Maybe Slack.UserId -> T.Text
getUserScreenName wsi = maybe (T.pack "<non-user>") $ getUserName wsi


parsePageNumber :: FilePath -> Integer
parsePageNumber = read . takeWhile isDigit . takeBaseName


putBetweenPreviousAndNext :: [a] -> [(Maybe a, a, Maybe a)]
putBetweenPreviousAndNext = go Nothing
 where
  go mbx (x1 : x2 : x3 : xs) =
    (mbx, x1, Just x2) : (Just x1, x2, Just x3) : go (Just x2) (x3 : xs)
  go mbx [x1, x2] =
    [(mbx, x1, Just x2), (Just x1, x2, Nothing)]
  go mbx [x1] =
    [(mbx, x1, Nothing)]
  go Nothing [] =
    []
  go _ [] =
    error "putBetweenPreviousAndNext: Impossible!"
