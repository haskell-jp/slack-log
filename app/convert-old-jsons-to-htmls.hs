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
import           Data.Traversable     (for)
import           SlackLog.Html
import           SlackLog.Types       (targetChannels)
import           SlackLog.Util        (readJsonFile)
import qualified System.Directory     as Dir

main :: IO ()
main = Dir.withCurrentDirectory "doc" $ do
  ws <- loadWorkspaceInfo "json"
  logConfig <- readJsonFile "json/.config.json"

  namesByChannel <- for (HM.keys $ targetChannels logConfig) $ \chanId -> do
    names <- convertJsonsInChannel ws chanId
    return (chanId, names)

  generateIndexHtml ws namesByChannel
