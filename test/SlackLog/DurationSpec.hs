{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}

module SlackLog.DurationSpec
  ( spec
  ) where

import qualified Data.Aeson            as Json
import qualified Data.ByteString.Lazy  as BL
import           Data.List             (isInfixOf)
import qualified Data.Text             as T
import qualified Data.Text.Encoding    as TE
import           Test.Hspec
import           Test.Hspec.QuickCheck
import           Test.QuickCheck       (arbitrary, forAll)
import qualified Test.QuickCheck       as QC

import           SlackLog.Duration


spec :: Spec
spec =
  describe "FromJSON instance of Duration" $ do
    context "Given a decimal string with 'm' prefixed" $ do
      prop "if the parsed decimal number is more than zero, returns minutes of NominalDiffTime" $ \(QC.Positive i) ->
        Json.decode' (tc i 'm') `shouldBe` Just (Duration (fromInteger (i * 60)))

    context "Given a decimal string with 'h' prefixed" $ do
      prop "if the parsed decimal number is more than zero, returns hours of NominalDiffTime" $ \(QC.Positive i) ->
        Json.decode' (tc i 'h') `shouldBe` Just (Duration (fromInteger (i * 60 * 60)))

    context "Given a decimal string with 'd' prefixed" $ do
      prop "if the parsed decimal number is more than zero, returns days of NominalDiffTime" $ \(QC.Positive i) ->
        Json.decode' (tc i 'd') `shouldBe` Just (Duration (fromInteger (i * 60 * 60 * 24)))

    context "Given a decimal string with 'w' prefixed" $ do
      prop "if the parsed decimal number is more than zero, returns weeks of NominalDiffTime" $ \(QC.Positive i) ->
        Json.decode' (tc i 'w') `shouldBe` Just (Duration (fromInteger (i * 60 * 60 * 24 * 7)))

    let gen = QC.elements "mhdw"
     in prop "if the parsed decimal number is zero, returns an error" . forAll gen
      $ \prefix -> do
        let Left actual = Json.eitherDecode' @Duration (tc 0 prefix)
        actual `shouldSatisfy` isInfixOf "duration must be positive"

    let gen = (,) <$> (QC.getNegative <$> arbitrary) <*> QC.elements "mhdw"
     in prop "if the parsed decimal number is negative, returns an error" . forAll gen
      $ \(i, prefix) -> do
        let Left actual = Json.eitherDecode' @Duration (tc i prefix)
        actual `shouldSatisfy` isInfixOf "input does not start with a digit"

    let gen = QC.elements "mhdw"
     in prop "if no decimal number given, returns an error" . forAll gen
      $ \prefix -> do
        let Left actual =
              Json.eitherDecode' @Duration . BL.fromStrict . TE.encodeUtf8 . T.pack $ '"' : prefix : "\""
        actual `shouldSatisfy` isInfixOf "input does not start with a digit"


tc :: Integer -> Char -> BL.ByteString
tc i prefix =
  BL.fromStrict . TE.encodeUtf8 $ "\"" <> T.pack (show i) <> T.singleton prefix <> "\""
