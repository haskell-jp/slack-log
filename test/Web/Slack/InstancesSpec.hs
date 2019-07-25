module Web.Slack.InstancesSpec
  ( spec
  ) where


import qualified Data.Aeson            as Json
import           Test.Hspec
import           Test.Hspec.QuickCheck
import qualified Web.Slack.Common      as Slack

import           Web.Slack.Instances   ()

spec :: Spec
spec =
  describe "ToJSON Instance of Slack.Message" $
    prop "the encoded json is decoded as " $ \msg -> do
      actual <- either fail return $ Json.eitherDecode $ Json.encode msg
      actual `shouldBe` (msg :: Slack.Message)
