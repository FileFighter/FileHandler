{-# LANGUAGE DeriveGeneric #-}

module Models.User where
import ClassyPrelude

import Data.Aeson

data User = User
  { userId :: Int,
    username :: String,
    groups :: [String]
  }
  deriving (Show, Generic)

instance FromJSON User

instance ToJSON User
