-- |

{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Models.Path where
import ClassyPrelude
    ( otherwise,
      ($),
      Show,
      Generic,
      Semigroup((<>)),
      unpack,
      String,
      ByteString,
      Text,
      intercalate,
      isPrefixOf)
import ClassyPrelude.Yesod ( ToJSON(toJSON) )
import Data.ByteString.Char8 (pack)



newtype Path = Path {
              path :: String
                 }
  deriving (Show, Generic)



instance ToJSON Path where
    toJSON (Path path) = toJSON $ addLeadingSlash path


toByteString :: Path -> ByteString
toByteString (Path path) = pack path


fromMultiPiece :: [Text] -> Path
fromMultiPiece pathPieces =  Path $unpack $ "/" <> intercalate "/" pathPieces

addLeadingSlash :: String -> String
addLeadingSlash path
  | "/" `isPrefixOf` path = path
  | otherwise = "/" <> path
