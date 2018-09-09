module SlackLog.PaginationSpec
  ( spec
  )
where


import           Data.List             (isPrefixOf)
import           Test.Hspec
import           Test.Hspec.QuickCheck
import qualified Test.QuickCheck       as QC

import           SlackLog.Pagination


spec :: Spec
spec = do
  describe "repaginate"
    $ modifyMaxSize (* 5)
    $ prop "splits into pages with N elements"
    $ \(QC.Positive n, baseName, QC.NonEmpty sourcePages) -> do
        let result = repaginate n 1 baseName (sourcePages :: [NamedPage Char])
        map namedPageName result
          `shouldSatisfy` all ((baseName ++ "/") `isPrefixOf`)

        let resultPages = map namedPagePage result

        concat resultPages `shouldBe` concatMap namedPagePage sourcePages

        case reverse resultPages of
          [] -> fail "Assertion failure: the resulted page has no elements!"
          (hd : tl) -> do
            tl `shouldSatisfy` all ((== n) . length)
            hd `shouldSatisfy` ((<= n) . length)

  describe "chooseLatestPageOf" $ do
    prop "returns file path with the latest number" $ \(QC.NonEmpty positiveNums) -> do
      let nums = map QC.getPositive positiveNums
          maxNum = maximum (nums :: [Integer])
          maxFile = show maxNum ++ ".json"
          numFiles = map ((++ ".json") . show) nums
      chooseLatestPageOf numFiles `shouldReturn` (maxFile, maxNum)

    it "for a file list containing non number, returns an error" $
      chooseLatestPageOf ["a.json"] `shouldBe` Nothing
