{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
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


import           Control.Monad           ((<=<))
import qualified Data.Aeson              as Json
import qualified Data.ByteString         as B
import           Data.Char               (isDigit)
import           Data.Foldable           (for_)
import qualified Data.HashMap.Strict     as HM
import           Data.List               (sortOn)
import           Data.Maybe              (fromMaybe)
import qualified Data.Text               as T
import qualified Data.Text.Encoding      as TE
import qualified Data.Time.Clock         as TC
import qualified Data.Time.Format        as TF
import qualified Data.Time.LocalTime     as LT
import qualified Data.Time.Zones         as TZ
import           Data.Traversable        (for)
import           Safe                    (lastMay)
import qualified System.Directory        as Dir
import           System.FilePath         (takeBaseName, (<.>), (</>))
import           System.IO               (hPrint, stderr)
import qualified Text.Mustache           as TM
import qualified Text.Mustache.Render    as TMR
import           Text.Mustache.Types     ((~>))
import qualified Web.Slack.Common        as Slack
import qualified Web.Slack.MessageParser as Slack
import           Witherable              (wither)

import           SlackLog.Types          (ChannelId, ChannelName, Config (..),
                                          TemplatePaths (..), UserId, UserName)
import           SlackLog.Util           (failWhenLeft, readJsonFile)


data PageInfo = PageInfo
  { currentPagePath  :: FilePath
  , previousPagePath :: Maybe FilePath
  , nextPagePath     :: Maybe FilePath
  , channelId        :: ChannelId
  } deriving (Eq, Show)

data WorkspaceInfo = WorkspaceInfo
  { userNameById         :: HM.HashMap UserId UserName
  , channelNameById      :: HM.HashMap ChannelId ChannelName
  , indexPageTemplate    :: TM.Template
  , messagesPageTemplate :: TM.Template
  , workspaceInfoName    :: T.Text
  , getTimeDiff          :: TC.UTCTime -> LT.TimeZone
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
  B.writeFile htmlPath . TE.encodeUtf8 =<< renderSlackMessages ws pg
 where
  cid = channelId pg
  currentPath = currentPagePath pg
  htmlPath = ensurePathIn "html" cid currentPath


generateIndexHtml :: WorkspaceInfo -> [(ChannelId, [FilePath])] -> IO ()
generateIndexHtml ws = B.writeFile "index.html" . TE.encodeUtf8 <=< renderIndexOfPages ws


data PageInfoForMustache = PageInfoForMustache
  { wsimWorkspaceName     :: !T.Text
  , wsimChannelId         :: !T.Text
  , wsimChannelScreenName :: !T.Text
  , wsimCurrentPageNumber :: !Integer
  , wsimPreviousPagePath  :: !(Maybe FilePath)
  , wsimNextPagePath      :: !(Maybe FilePath)
  , wsimIndexPagePath     :: FilePath
  , wsimMessages          :: ![MessageForMustache]
  }

instance TM.ToMustache PageInfoForMustache where
  toMustache PageInfoForMustache {..} =
    TM.object
      [ "workspaceName" ~> wsimWorkspaceName
      , "channelId" ~> wsimChannelId
      , "channelScreenName" ~> wsimChannelScreenName
      , "currentPageNumber" ~> wsimCurrentPageNumber
      , "previousPagePath" ~> wsimPreviousPagePath
      , "nextPagePath" ~> wsimNextPagePath
      , "indexPagePath" ~> wsimIndexPagePath
      , "messages" ~> wsimMessages
      ]


data MessageForMustache = MessageForMustache
  { mfmUserScreenName  :: !T.Text
  , mfmHtmlMessageBody :: !T.Text
  , mfmTs              :: !T.Text
  , mfmFormattedTs     :: !T.Text
  }

instance TM.ToMustache MessageForMustache where
  toMustache MessageForMustache {..} =
    TM.object
      [ "userScreenName" ~> mfmUserScreenName
      , "htmlMessageBody" ~> mfmHtmlMessageBody
      , "ts" ~> mfmTs
      , "formattedTs" ~> mfmFormattedTs
      ]


renderSlackMessages :: WorkspaceInfo -> PageInfo -> IO T.Text
renderSlackMessages ws@WorkspaceInfo {..} p@PageInfo {..} =
  printingWarning . render =<< readJsonFile (ensurePathIn "json" channelId currentPagePath)
 where
  render :: [Slack.Message] -> ([TMR.SubstitutionError], T.Text)
  render =
    TM.checkedSubstitute messagesPageTemplate . pageInfoForMustache ws p


pageInfoForMustache :: WorkspaceInfo -> PageInfo -> [Slack.Message] -> PageInfoForMustache
pageInfoForMustache ws@WorkspaceInfo {..} PageInfo {..} msgs = PageInfoForMustache
  { wsimWorkspaceName     = workspaceInfoName
  , wsimChannelId         = channelId
  , wsimChannelScreenName = getChannelScreenName ws channelId
  , wsimCurrentPageNumber = parsePageNumber currentPagePath
  , wsimPreviousPagePath  = ("../../" ++) . ensurePathIn "html" channelId <$> previousPagePath
  , wsimNextPagePath      = ("../../" ++) . ensurePathIn "html" channelId <$> nextPagePath
  , wsimIndexPagePath = "../../"
  , wsimMessages          = map (messageForMustache ws) msgs
  }


messageForMustache :: WorkspaceInfo -> Slack.Message -> MessageForMustache
messageForMustache ws Slack.Message {..} =
  MessageForMustache
    { mfmUserScreenName = getUserScreenName ws messageUser
    , mfmHtmlMessageBody = mkMessageBody ws messageText
    , mfmTs = Slack.slackTimestampTs messageTs
    , mfmFormattedTs = T.pack . timestampBlock $ Slack.slackTimestampTime messageTs
    }
 where
  timestampBlock tm =
    let lt = LT.utcToZonedTime (getTimeDiff ws tm) tm
    in TF.formatTime TF.defaultTimeLocale "%Y-%m-%d %T %z" lt


loadWorkspaceInfo :: Config -> FilePath -> IO WorkspaceInfo
loadWorkspaceInfo cfg dir = do
  userNameById <- failWhenLeft =<< Json.eitherDecodeFileStrict' (dir </> ".users.json")
  let channelNameById = targetChannels cfg
      TemplatePaths { indexPage, messagesPage } = templatePaths cfg
      workspaceInfoName = workspaceName cfg
  getTimeDiff <- fmap TZ.timeZoneForUTCTime . TZ.loadTZFromDB $ timeZone cfg

  indexPageTemplate <-
    either (fail . show) return =<< TM.localAutomaticCompile indexPage
  messagesPageTemplate <-
    either (fail . show) return =<< TM.localAutomaticCompile messagesPage

  return WorkspaceInfo {..}


data IndexPageData = IndexPageData
  { ipdWorkspaceName :: !T.Text
  , ipdIndexEntries  :: ![IndexEntry]
  }

instance TM.ToMustache IndexPageData where
  toMustache IndexPageData {..} =
    TM.object
      [ "workspaceName" ~> ipdWorkspaceName
      , "indexEntries" ~> ipdIndexEntries
      ]


data IndexEntry = IndexEntry
  { ieChannelId         :: !ChannelId
  , ieChannelScreenName :: !T.Text
  , ieLastMessageTs     :: !String
  , ieLastJsonPath      :: !FilePath
  , iePageSummaries     :: ![PageSummary]
  }

instance TM.ToMustache IndexEntry where
  toMustache IndexEntry {..} =
    TM.object
      [ "channelId" ~> ieChannelId
      , "channelScreenName" ~> ieChannelScreenName
      , "lastMessageTs" ~> ieLastMessageTs
      , "lastJsonPath" ~> ieLastJsonPath
      , "pageSummaries" ~> iePageSummaries
      ]

data PageSummary = PageSummary
  { psHtmlPath                   :: !FilePath
  , psPageNumber                 :: !Integer
  , psFirstMessageUserScreenName :: !T.Text
  , psFirstMessageTextTruncated  :: !T.Text
  , psFirstMessageTs             :: !String
  }

instance TM.ToMustache PageSummary where
  toMustache PageSummary {..} =
    TM.object
      [ "htmlPath" ~> psHtmlPath
      , "pageNumber" ~> psPageNumber
      , "firstMessageUserScreenName" ~> psFirstMessageUserScreenName
      , "firstMessageTextTruncated" ~> psFirstMessageTextTruncated
      , "firstMessageTs" ~> psFirstMessageTs
      ]


renderIndexOfPages :: WorkspaceInfo -> [(ChannelId, [FilePath])] -> IO T.Text
renderIndexOfPages ws@WorkspaceInfo {..} cidJsonPaths = do
  ies <- (`wither` cidJsonPaths) $ \(cid, jsonPaths) -> do
    let sortedJsonPaths = sortOn parsePageNumber jsonPaths
    case lastMay sortedJsonPaths of
        Just lastPath -> do
          lastLastMessage <- readLastMessage $ ensurePathIn "json" cid lastPath
          pss <- for sortedJsonPaths $ \path -> do
            firstMessage <- readFirstMessage (ensurePathIn "json" cid path)
            return $ PageSummary
              { psHtmlPath = ensurePathIn "html" cid path
              , psPageNumber = parsePageNumber path
              , psFirstMessageUserScreenName =
                  getUserScreenName ws $ Slack.messageUser firstMessage
              , psFirstMessageTextTruncated =
                  unescapeHtmlEntities . truncatedMessage $ Slack.messageText firstMessage
              , psFirstMessageTs =
                  timestampWords . Slack.slackTimestampTime $ Slack.messageTs firstMessage
              }
          return . Just $ IndexEntry
            { ieChannelId = cid
            , ieChannelScreenName = getChannelScreenName ws cid
            , ieLastMessageTs =
                timestampWords $ Slack.slackTimestampTime $ Slack.messageTs lastLastMessage
            , ieLastJsonPath = ensurePathIn "html" cid lastPath
            , iePageSummaries = pss
            }
        Nothing ->
          return Nothing
  printingWarning . render $ IndexPageData workspaceInfoName ies
 where
  render :: IndexPageData -> ([TMR.SubstitutionError], T.Text)
  render = TM.checkedSubstitute indexPageTemplate
  timestampWords tm =
    let lt = LT.utcToZonedTime (getTimeDiff tm) tm
    in TF.formatTime TF.defaultTimeLocale "%Y-%m-%d %T %z" lt

  readFirstMessage :: FilePath -> IO Slack.Message
  readFirstMessage = fmap head . readJsonFile

  readLastMessage :: FilePath -> IO Slack.Message
  readLastMessage = fmap last . readJsonFile

{-
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
          (unescapeHtmlEntities $ truncatedMessage messageText)
        )
      )
-}

mkMessageBody :: WorkspaceInfo -> Slack.SlackMessageText -> T.Text
mkMessageBody =
  Slack.messageToHtml Slack.defaultHtmlRenderers . getUserName


truncatedMessageMaxLength :: Int
truncatedMessageMaxLength = 150

truncatedMessage :: Slack.SlackMessageText -> T.Text
truncatedMessage Slack.SlackMessageText { unSlackMessageText = msg }
  | T.length msg > truncatedMessageMaxLength
  = T.take truncatedMessageMaxLength msg <> T.pack "..."
  | otherwise = msg

unescapeHtmlEntities :: T.Text -> T.Text
unescapeHtmlEntities =
  T.replace (T.pack "&lt;") (T.pack "<")
  . T.replace (T.pack "&gt;") (T.pack ">")
  . T.replace (T.pack "&amp;") (T.pack "&")

ensurePathIn :: String -> ChannelId -> FilePath -> FilePath
ensurePathIn typ cid name = typ ++ "/" ++ T.unpack cid ++ "/" ++ takeBaseName name <.> typ


getChannelScreenName :: WorkspaceInfo -> ChannelId -> ChannelName
getChannelScreenName WorkspaceInfo {..} cid  =
  fromMaybe cid $ HM.lookup cid channelNameById


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


printingWarning :: ([TMR.SubstitutionError], T.Text) -> IO T.Text
printingWarning (ws, t) = do
  for_ ws (hPrint stderr)
  return t
