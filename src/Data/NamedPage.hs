{-# LANGUAGE ScopedTypeVariables #-}

module Data.NamedPage
  ( NamedPage(..)
  , Name
  , repaginate
  )
where


import           Data.List.Extra (chunksOf)
import qualified Test.QuickCheck as QC


type Name = String


data NamedPage a = NamedPage
  { namedPageName :: !Name
  , namedPagePage :: ![a]
  } deriving (Eq, Show)

instance QC.Arbitrary a => QC.Arbitrary (NamedPage a) where
  arbitrary = NamedPage <$> QC.arbitrary <*> (QC.getNonEmpty <$> QC.arbitrary)


repaginate :: forall a. Int -> Name -> [NamedPage a] -> [NamedPage a]
repaginate n baseName = zipWith toNamedPage [1..] . chunksOf n . concatMap namedPagePage
 where
  toNamedPage :: Int -> [a] -> NamedPage a
  toNamedPage pn =
    NamedPage (baseName ++ "-" ++ show pn)
