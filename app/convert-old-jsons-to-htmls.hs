{-
Copyright 2018 Japan Haskell User Group

Licensed under the Apache License, Version 2.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.
-}

import qualified Data.HashMap.Strict  as HM
import           Data.List            (sortOn)
import qualified Data.Text            as T
import           Data.Traversable     (for)
import           SlackLog.Html
import           SlackLog.Types       (targetChannels)
import           SlackLog.Util        (readJsonFile)
import qualified System.Directory     as Dir
import           System.FilePath      ((</>))

main :: IO ()
main = Dir.withCurrentDirectory "doc" $ do
  ws <- loadWorkspaceInfo "json"
  logConfig <- readJsonFile "json/.config.json"

  namesByChannel <- for (HM.toList $ targetChannels logConfig) $ \(chanId, _vis) -> do
    let channelIdStr = T.unpack chanId

    Dir.createDirectoryIfMissing True $ "html" </> channelIdStr

    putStrLn channelIdStr

    triples
      <- putBetweenPreviousAndNext
      .   sortOn parsePageNumber
      <$> Dir.listDirectory ("json" </> channelIdStr)
    names <- for triples $ \(mPrev, name, mNext) -> do
      let pg = PageInfo name mPrev mNext chanId
      putStrLn $ "  " ++ show pg
      convertToHtmlFile ws pg
      return name

    return (chanId, names)

  generateIndexHtml ws namesByChannel


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
