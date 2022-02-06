{-# LANGUAGE DeriveGeneric #-}
-- |


module Models.RestApiStatus where

import ClassyPrelude

import Data.Aeson

data RestApiStatus = RestApiStatus
  { message :: !String,
    status :: !String
  }
  deriving (Show, Generic)

instance FromJSON RestApiStatus

instance ToJSON RestApiStatus
