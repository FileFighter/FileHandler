{-# LANGUAGE DeriveGeneric #-}

module Models.User where

import ClassyPrelude
import ClassyPrelude (Eq)
import Data.Aeson

data User = User
  { userId :: Int,
    username :: String,
    privileges :: String
  }
  deriving (Show, Generic, Eq)

userIdFieldRename :: String -> String
userIdFieldRename "userId" = "id"
userIdFieldRename "id" = "userId"
userIdFieldRename name = name

instance ToJSON User where
  toJSON =
    genericToJSON
      defaultOptions
        { fieldLabelModifier = userIdFieldRename,
          omitNothingFields = True
        }

instance FromJSON User where
  parseJSON =
    genericParseJSON
      defaultOptions
        { fieldLabelModifier = userIdFieldRename,
          omitNothingFields = True
        }
