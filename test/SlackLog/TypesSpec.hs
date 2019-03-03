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

{-# LANGUAGE OverloadedStrings #-}

module SlackLog.TypesSpec
  ( spec
  ) where

import qualified Data.Aeson            as Json
import           Data.Monoid           ((<>))
import qualified Data.Text             as T
import qualified Data.Text.Encoding    as TE
import           Test.Hspec
import           Test.Hspec.QuickCheck
import qualified Test.QuickCheck       as QC

import           SlackLog.Types


spec :: Spec
spec =
  describe "FromJSON instance of Visibility" $
    let g :: QC.Gen (Visibility, T.Text, [T.Text])
        g = do
          let chars = filter (\c -> c /= '"' && c /= '\\') ['!'..'~']
              --             ^ Avoid characters required to escape
              word = T.pack <$> QC.listOf1 (QC.elements chars)
          (,,)
            <$> QC.elements [Private, Public]
            <*> word
            <*> QC.listOf1 word
     in prop "parses only the first word" $ QC.forAll g $ \(vis, invalidWord, leftWords) -> do
        let visTxt       = T.pack $ show vis
            validWords   = TE.encodeUtf8 ("\"" <> T.unwords (visTxt      : leftWords) <> "\"")
            invalidWords = TE.encodeUtf8 ("\"" <> T.unwords (invalidWord : leftWords) <> "\"")

        Json.decodeStrict validWords `shouldBe` Just vis
        Json.decodeStrict invalidWords `shouldBe` (Nothing :: Maybe Visibility)
