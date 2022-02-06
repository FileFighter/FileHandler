{-# LANGUAGE DeriveGeneric #-}

module Models.Inode where

import ClassyPrelude
import Data.Aeson
import Models.User

data Inode = Inode
  { fileSystemId :: !Int,
    name :: String,
    path :: Maybe String,
    size :: Int,
    owner :: User,
    lastUpdatedBy :: User,
    lastUpdated :: Int,
    mimeType :: Maybe String,
    filesystemType :: String,
    shared :: Bool
  }
  deriving (Show, Generic)

typeFieldRename :: String -> String
typeFieldRename "filesystemType" = "type"
typeFieldRename "type" = "filesystemType"
typeFieldRename name = name

instance FromJSON Inode where
  parseJSON =
    genericParseJSON
      defaultOptions
        { fieldLabelModifier = typeFieldRename,
          omitNothingFields = True
        }
