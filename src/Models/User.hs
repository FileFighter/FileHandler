{-# LANGUAGE DeriveGeneric #-}

module Models.User where
import Data.Aeson
import GHC.Generics

data User = User
  { userId :: Int,
    username :: String,
    groups :: [String]
  }
  deriving (Show, Generic)

instance FromJSON User

instance ToJSON User
