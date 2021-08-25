{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NamedFieldPuns             #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE StrictData                 #-}
{-# LANGUAGE TupleSections              #-}

-- | Assumes functions in this module are executed in doc/ directory

module SlackLog.Html
  ( convertJsonsInChannel
  , generateIndexHtml
  , collectTargetJsons
  , renderSlackMessages
  , renderThread
  , renderIndexOfPages
  , loadWorkspaceInfo
  , parsePageNumber
  , PageInfo(..)
  , WorkspaceInfo(..)
  ) where


import           Control.Monad           (unless, (<=<))
import qualified Data.Aeson              as Json
import qualified Data.ByteString         as B
import           Data.Char               (isDigit)
import           Data.Foldable           (for_)
import qualified Data.HashMap.Strict     as HM
import           Data.List               (isSuffixOf, sortOn)
import           Data.Maybe              (fromMaybe, mapMaybe)
import qualified Data.Text               as T
import qualified Data.Text.Encoding      as TE
import qualified Data.Time.Clock         as TC
import qualified Data.Time.Clock.POSIX   as TCP
import qualified Data.Time.Format        as TF
import qualified Data.Time.LocalTime     as LT
import qualified Data.Time.Zones         as TZ
import           Data.Traversable        (for)
import           Safe                    (lastMay)
import qualified System.Directory        as Dir
import           System.FilePath         (dropExtension, takeBaseName, (<.>),
                                          (</>))
import           System.IO               (hPrint, stderr)
import qualified Text.Mustache           as TM
import qualified Text.Mustache.Render    as TMR
import           Text.Mustache.Types     (Value (Object), (~>))
import qualified Web.Slack.Common        as Slack
import qualified Web.Slack.MessageParser as Slack
import           Witherable              (wither)

import           SlackLog.Types          (ChannelId, ChannelName, Config (..),
                                          TemplatePaths (..), UserId, UserName)
import           SlackLog.Util           (failWhenLeft, readJsonFile)

import           Prelude                 hiding (pi)

data PageInfo = PageInfo
  { pageNumber       :: Integer
  , thisPagePath     :: FilePath
  , previousPagePath :: Maybe FilePath
  , nextPagePath     :: Maybe FilePath
  , channelId        :: ChannelId
  } deriving (Eq, Show)

data WorkspaceInfo = WorkspaceInfo
  { userNameById         :: HM.HashMap UserId UserName
  , channelNameById      :: HM.HashMap ChannelId ChannelName
  , indexPageTemplate    :: TM.Template
  , messagesPageTemplate :: TM.Template
  , threadPageTemplate   :: TM.Template
  , workspaceInfoName    :: T.Text
  , getTimeDiff          :: TC.UTCTime -> LT.TimeZone
  }


collectTargetJsons :: ChannelId -> IO [FilePath]
collectTargetJsons =
  fmap (filter (".json" `isSuffixOf`)) . Dir.listDirectory . ("json" </>) . T.unpack


convertJsonsInChannel :: WorkspaceInfo -> ChannelId -> [FilePath] -> IO ()
convertJsonsInChannel ws chanId jsonPaths = do
  let channelIdStr = T.unpack chanId

  let channelHtmlDir = "html" </> channelIdStr
  Dir.createDirectoryIfMissing True channelHtmlDir

  putStrLn channelIdStr

  let triples =
        putBetweenPreviousAndNext
          . sortOn fst
          $ map (\jsonPath -> (parsePageNumber jsonPath, jsonPath)) jsonPaths

  for_ triples $ \(mPrev, (pn, name), mNext) -> do
    let pg = PageInfo pn name (snd <$> mPrev) (snd <$> mNext) chanId
    putStrLn $ "  " ++ show pg
    convertToHtmlFile ws pg

    let threadsHtmlDir = channelHtmlDir </> show (pageNumber pg)
    Dir.createDirectoryIfMissing True threadsHtmlDir

    convertToRepliesHtmlFiles ws pg


convertToRepliesHtmlFiles :: WorkspaceInfo -> PageInfo -> IO ()
convertToRepliesHtmlFiles ws pg = do
  threadJsonNames <- collectThreadJsonNames pg
  for_ threadJsonNames $ \threadJsonName -> do
    let htmlPath = ensureThreadPathIn "html" cid currentPagePath $ takeBaseName threadJsonName
    B.writeFile htmlPath . TE.encodeUtf8 =<< renderThread ws pg threadJsonName

 where
  cid = channelId pg
  currentPagePath = thisPagePath pg


collectThreadJsonNames :: PageInfo -> IO [FilePath]
collectThreadJsonNames PageInfo {pageNumber, channelId} = do
  putStrLn $ "Collecting Channel " ++ T.unpack channelId ++ "'s threads at page#" ++ show pageNumber
  let chanDir = "json" </> T.unpack channelId
      prependChanDirIfPageFound d =
        if d == show pageNumber
          then Just $ chanDir </> d
          else Nothing
  pageDirs <-
    mapMaybe prependChanDirIfPageFound <$> Dir.listDirectory chanDir
  unless (null pageDirs) .
    putStrLn $ "Found page directories: " ++ show pageDirs ++ " in " ++ T.unpack channelId
  filter (".json" `isSuffixOf`) . concat <$> mapM Dir.listDirectory pageDirs


convertToHtmlFile :: WorkspaceInfo -> PageInfo -> IO ()
convertToHtmlFile ws pg@PageInfo{channelId, thisPagePath} =
  B.writeFile htmlPath . TE.encodeUtf8 =<< renderSlackMessages ws pg
 where
  htmlPath = ensurePathIn "html" channelId thisPagePath


generateIndexHtml :: WorkspaceInfo -> [(ChannelId, [FilePath])] -> IO ()
generateIndexHtml ws = B.writeFile "index.html" . TE.encodeUtf8 <=< renderIndexOfPages ws


newtype HasReplies = HasReplies Bool deriving (Eq, Show, TM.ToMustache)


data PageInfoForMustache = PageInfoForMustache
  { pifmWorkspaceName     :: !T.Text
  , pifmChannelId         :: !T.Text
  , pifmChannelScreenName :: !T.Text
  , pifmCurrentPageNumber :: !Integer
  , pifmPreviousPagePath  :: !(Maybe FilePath)
  , pifmNextPagePath      :: !(Maybe FilePath)
  , pifmIndexPagePath     :: FilePath
  , pifmMessages          :: ![MessageForMustache]
  }

instance TM.ToMustache PageInfoForMustache where
  toMustache PageInfoForMustache {..} =
    TM.object
      [ "workspaceName" ~> pifmWorkspaceName
      , "channelId" ~> pifmChannelId
      , "channelScreenName" ~> pifmChannelScreenName
      , "currentPageNumber" ~> pifmCurrentPageNumber
      , "previousPagePath" ~> pifmPreviousPagePath
      , "nextPagePath" ~> pifmNextPagePath
      , "indexPagePath" ~> pifmIndexPagePath
      , "messages" ~> pifmMessages
      ]


data MessageForMustache = MessageForMustache
  { mfmUserScreenName  :: !T.Text
  , mfmHtmlMessageBody :: !T.Text
  , mfmTs              :: !T.Text
  , mfmFormattedTs     :: !T.Text
  , mfmPathToReplies   :: !T.Text
  , mfmHasReplies      :: !HasReplies
  }

instance TM.ToMustache MessageForMustache where
  toMustache MessageForMustache {..} =
    TM.object
      [ "userScreenName" ~> mfmUserScreenName
      , "htmlMessageBody" ~> mfmHtmlMessageBody
      , "ts" ~> mfmTs
      , "formattedTs" ~> mfmFormattedTs
      , "pathToReplies" ~> mfmPathToReplies
      , "hasReplies" ~> mfmHasReplies
      ]


renderSlackMessages :: WorkspaceInfo -> PageInfo -> IO T.Text
renderSlackMessages ws@WorkspaceInfo {..} p@PageInfo {..} =
  printingWarning
    . render
    =<< mapM (\m -> (,) <$> hasReplies channelId thisPagePath m <*> pure m)
    =<< readJsonFile jsonPath
 where
  render =
    TM.checkedSubstitute messagesPageTemplate . pageInfoForMustache ws p
  jsonPath = ensurePathIn "json" channelId thisPagePath


hasReplies :: ChannelId -> FilePath -> Slack.Message -> IO HasReplies
hasReplies cid pagePath Slack.Message { messageTs } =
  HasReplies <$> Dir.doesFileExist threadFilePath
 where
  threadFilePath =
    ensureThreadPathIn "json" cid pagePath $ T.unpack (Slack.slackTimestampTs messageTs)


pageInfoForMustache :: WorkspaceInfo -> PageInfo -> [(HasReplies, Slack.Message)] -> PageInfoForMustache
pageInfoForMustache ws@WorkspaceInfo {..} pi@PageInfo {..} msgs = PageInfoForMustache
  { pifmWorkspaceName     = workspaceInfoName
  , pifmChannelId         = channelId
  , pifmChannelScreenName = getChannelScreenName ws channelId
  , pifmCurrentPageNumber = parsePageNumber thisPagePath
  , pifmPreviousPagePath  = ("../../" ++) . ensurePathIn "html" channelId <$> previousPagePath
  , pifmNextPagePath      = ("../../" ++) . ensurePathIn "html" channelId <$> nextPagePath
  , pifmIndexPagePath     = "../../"
  , pifmMessages          = map (messageForMustache ws pi) msgs
  }


messageForMustache :: WorkspaceInfo -> PageInfo -> (HasReplies, Slack.Message) -> MessageForMustache
messageForMustache ws pi (hasR, Slack.Message {..}) =
  MessageForMustache
    { mfmUserScreenName = getUserScreenName ws messageUser
    , mfmHtmlMessageBody = mkMessageBody ws messageText
    , mfmTs = tst
    , mfmFormattedTs = T.pack . timestampBlock ws $ Slack.slackTimestampTime messageTs
    , mfmPathToReplies =
        T.pack (show (pageNumber pi)) <> "/" <> tst <> ".html"
    , mfmHasReplies = hasR
    }
 where
  tst = Slack.slackTimestampTs messageTs


timestampBlock :: WorkspaceInfo -> TC.UTCTime -> String
timestampBlock ws tm =
  let lt = LT.utcToZonedTime (getTimeDiff ws tm) tm
  in TF.formatTime TF.defaultTimeLocale "%Y-%m-%d %T %z" lt


data ThreadForMustache = ThreadForMustache
  { tfmThreadFormattedTs :: !T.Text
  , tfmPathToParentPage  :: !T.Text
  , tfmParentPage        :: PageInfoForMustache
  }

instance TM.ToMustache ThreadForMustache where
  toMustache ThreadForMustache {..} =
    -- Merge with PageInfoForMustache to flatten the object
    Object $ tfm <> pifm
   where
    tfm = HM.fromList
      [ "threadFormattedTs" ~> tfmThreadFormattedTs
      , "pathToParentPage" ~> tfmPathToParentPage
      ]
    pifm =
      case TM.toMustache tfmParentPage of
          Object o -> o
          other    -> error $ "Unexpected Mustache Value: " ++ show other


threadForMustache :: WorkspaceInfo -> PageInfo -> FilePath -> [Slack.Message] -> ThreadForMustache
threadForMustache ws pi@PageInfo {pageNumber} threadJsonName msgs = ThreadForMustache
  { tfmThreadFormattedTs = T.pack . timestampBlock ws $ tsFromThreadJsonFileName threadJsonName
  , tfmPathToParentPage  = "../" <> T.pack (show pageNumber) <> ".html"
  , tfmParentPage        = pageInfoForMustache ws pi $ map (HasReplies False,) msgs
  }


tsFromThreadJsonFileName :: FilePath -> TC.UTCTime
tsFromThreadJsonFileName =
  -- Valid strings for the (Read NominalDiffTime) instance must be prefixed with "s".
  TCP.posixSecondsToUTCTime . read . (++ "s") . dropExtension


renderThread :: WorkspaceInfo -> PageInfo -> FilePath -> IO T.Text
renderThread ws@WorkspaceInfo {..} p@PageInfo {..} threadJsonName =
  printingWarning
    . render
    =<< readJsonFile jsonPath
 where
  render :: [Slack.Message] -> ([TMR.SubstitutionError], T.Text)
  render =
    TM.checkedSubstitute threadPageTemplate . threadForMustache ws p threadJsonName
  jsonPath = ensureThreadPathIn "json" channelId thisPagePath (takeBaseName threadJsonName)


loadWorkspaceInfo :: Config -> FilePath -> IO WorkspaceInfo
loadWorkspaceInfo cfg dir = do
  userNameById <- failWhenLeft =<< Json.eitherDecodeFileStrict' (dir </> ".users.json")
  let channelNameById = targetChannels cfg
      TemplatePaths
        { indexPage
        , messagesPage
        , threadPage
        } = templatePaths cfg
      workspaceInfoName = workspaceName cfg
  getTimeDiff <- fmap TZ.timeZoneForUTCTime . TZ.loadTZFromDB $ timeZone cfg

  indexPageTemplate <-
    either (fail . show) return =<< TM.localAutomaticCompile indexPage
  messagesPageTemplate <-
    either (fail . show) return =<< TM.localAutomaticCompile messagesPage
  threadPageTemplate <-
    either (fail . show) return =<< TM.localAutomaticCompile threadPage

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


ensureThreadPathIn :: String -> ChannelId -> FilePath -> FilePath -> FilePath
ensureThreadPathIn typ cid pagePath tsStr =
  typ
    </> T.unpack cid
    </> takeBaseName pagePath
    </> tsStr
    <.> typ


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
