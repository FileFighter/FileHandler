{-# LANGUAGE DeriveGeneric #-}
-- |

module Models.RestApiStatus where

import Data.Aeson
import GHC.Generics

data RestApiStatus = RestApiStatus
  { message :: !String,
    status :: !String
  }
  deriving (Show, Generic)

instance FromJSON RestApiStatus

instance ToJSON RestApiStatus
