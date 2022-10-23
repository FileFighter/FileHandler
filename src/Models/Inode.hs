{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Redundant bracket" #-}

module Models.Inode where

import ClassyPrelude
import Data.Aeson
import Data.Text as T (pack, splitOn, unpack)
import Models.Path (Path)
import Models.User

data Inode = Inode
  { fileSystemId :: String,
    name :: String,
    path :: Maybe String,
    mimeType :: Maybe String,
    size :: Int,
    lastUpdated :: Int,
    lastUpdatedBy :: User
  }
  deriving (Show, Generic, Eq)

typeFieldRename :: String -> String
typeFieldRename "fileSystemId" = "id"
typeFieldRename "id" = "fileSystemId"
typeFieldRename name = name

instance FromJSON Inode where
  parseJSON =
    genericParseJSON
      defaultOptions
        { fieldLabelModifier = typeFieldRename,
          omitNothingFields = True
        }

instance ToJSON Inode where
  toJSON =
    genericToJSON
      defaultOptions
        { fieldLabelModifier = typeFieldRename,
          omitNothingFields = True
        }

getFirstPathPiece :: Inode -> String
getFirstPathPiece inode = do
  let inodePath = path inode
  let path = T.pack $ fromMaybe (name inode) (inodePath)
  case (filter (/= "") $ splitOn "/" path) of
    [] -> name inode
    firstPathPiece : rest -> T.unpack firstPathPiece
