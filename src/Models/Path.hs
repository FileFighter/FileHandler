-- |

{-# LANGUAGE DeriveGeneric #-}
module Models.Path where
import ClassyPrelude
import ClassyPrelude.Yesod



newtype Path = Path {
              path :: String
                 }
  deriving (Show, Generic)



instance ToJSON Path where
    toJSON (Path path) = toJSON $ addLeadingSlash path

addLeadingSlash :: String -> String
addLeadingSlash path
  | "/" `isPrefixOf` path = path
  | otherwise = "/" <> path
