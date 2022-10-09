{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Redundant bracket" #-}

-- |
module Settings where

import ClassyPrelude.Yesod
  ( ByteString,
    FromJSON,
    Generic,
    Int,
    Maybe (..),
    Semigroup ((<>)),
    Show (show),
    String,
    Value,
    either,
    id,
    ($),
    (<$>),
  )
import qualified Control.Exception as Exception
import Data.Aeson
  ( Result (..),
    fromJSON,
    withObject,
    (.!=),
    (.:?),
  )
import Data.FileEmbed (embedFile)
import Data.Yaml (decodeEither')
import Database.MongoDB (Password)
import Database.Persist.MongoDB (MongoAuth (MongoAuth), MongoConf (mgAuth))
import GHC.Generics ()
import Models.User (User (username))
import Network.Wai.Handler.Warp (HostPreference)
import Yesod.Default.Config2 (applyEnvValue, configSettingsYml)

type EncryptionPassword = Maybe String

instance {-# OVERLAPS #-} Show EncryptionPassword where
  show Nothing = "Not using encryption"
  show (Just password) = "Using encryption with the specified password"

data AppSettings = AppSettings
  { appProfile :: String,
    appDatabaseConf :: MongoConf,
    fileSystemServiceSettings :: FileSystemServiceSettings,
    encryptionPassword :: EncryptionPassword,
    frontendOrigin :: String
  }
  deriving (Generic)

instance FromJSON AppSettings

instance Show AppSettings where
  show (AppSettings appProfile appDatabaseConf fileSystemServiceSettings encryptionPassword frontendOrigin) =
    "Profile: " <> appProfile <> "\n"
      <> "DB conf: "
      <> show (hidePasswordInMongoConf appDatabaseConf)
      <> "\n"
      <> "FSS Config: "
      <> show fileSystemServiceSettings
      <> "\n"
      <> "frontend origin: "
      <> frontendOrigin
      <> "\n"
      <> "Encryption Settings: "
      <> show encryptionPassword

hidePasswordInMongoConf :: MongoConf -> MongoConf
hidePasswordInMongoConf conf = conf {mgAuth = (overwritePassword <$> mgAuth conf)}
  where
    overwritePassword (MongoAuth user _) = MongoAuth user "****"

data FileSystemServiceSettings = FileSystemServiceSettings
  { url :: String,
    port :: Int
  }
  deriving (Generic, Show)

instance FromJSON FileSystemServiceSettings

-- | Raw bytes at compile time of @config/settings.yml@
configSettingsYmlBS :: ByteString
configSettingsYmlBS = $(embedFile configSettingsYml)

-- | @config/settings.yml@, parsed to a @Value@.
configSettingsYmlValue :: Value
configSettingsYmlValue =
  either Exception.throw id $
    decodeEither' configSettingsYmlBS
