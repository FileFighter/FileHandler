{-# LANGUAGE DeriveGeneric #-}

module Models.Inode where

import ClassyPrelude
import Data.Aeson
import Models.User
import Models.Path (Path)

data Inode = Inode
  { fileSystemId :: String,
    name :: String,
    path :: Maybe String,
    mimeType :: Maybe String,
    size :: Int,
    lastUpdated :: Int,
    lastUpdatedBy :: User
  }
  deriving (Show, Generic)

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
