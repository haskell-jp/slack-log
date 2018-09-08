module Data.NamedPageSpec
  ( spec
  )
where


import           Data.List             (isPrefixOf)
import           Test.Hspec
import           Test.Hspec.QuickCheck
import qualified Test.QuickCheck       as QC

import           Data.NamedPage


spec :: Spec
spec =
  describe "repaginate"
    $ modifyMaxSize (* 5)
    $ prop "splits into pages with N elements"
    $ \(QC.Positive n, baseName, QC.NonEmpty sourcePages) -> do
        let result = repaginate n baseName (sourcePages :: [NamedPage Char])
        map namedPageName result
          `shouldSatisfy` all ((baseName ++ "-") `isPrefixOf`)

        let resultPages = map namedPagePage result

        concat resultPages `shouldBe` concatMap namedPagePage sourcePages

        case reverse resultPages of
          [] -> fail "Assertion failure: the resulted page has no elements!"
          (hd : tl) -> do
            tl `shouldSatisfy` all ((== n) . length)
            hd `shouldSatisfy` ((<= n) . length)
