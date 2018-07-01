module Web.Slack.Instances.Lib
  ( deriveToJsonWithoutTypeNamePrefixUnderscore
  ) where


import qualified Data.Aeson          as Json
import           Data.Aeson.TH       (deriveToJSON)
import           Data.Aeson.Types    (Options (fieldLabelModifier), camelTo2)
import           Data.Char           (toLower)
import           Language.Haskell.TH (Dec, Name, Q, nameBase)


deriveToJsonWithoutTypeNamePrefixUnderscore :: Name -> Q [Dec]
deriveToJsonWithoutTypeNamePrefixUnderscore name =
  deriveToJSON Json.defaultOptions { fieldLabelModifier = map toLower . camelTo2 '_' . drop (length $ nameBase name) } name
