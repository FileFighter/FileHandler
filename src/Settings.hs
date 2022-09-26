{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- |
module Settings where

import ClassyPrelude.Yesod
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
import Database.Persist.MongoDB (MongoConf)
import GHC.Generics
import Network.Wai.Handler.Warp (HostPreference)
import Yesod.Default.Config2 (applyEnvValue, configSettingsYml)
import Yesod.Default.Util
  ( WidgetFileSettings,
    widgetFileNoReload,
    widgetFileReload,
  )

data AppSettings = AppSettings
  { appProfile :: String,
    appDatabaseConf :: MongoConf,
    fileSystemServiceSettings :: FileSystemServiceSettings,
    encryptionPassword :: Maybe String
  }
  deriving (Generic)

instance FromJSON AppSettings

data FileSystemServiceSettings = FileSystemServiceSettings
  { url :: String,
    port :: Int
  }
  deriving (Generic)

instance FromJSON FileSystemServiceSettings

-- | Raw bytes at compile time of @config/settings.yml@
configSettingsYmlBS :: ByteString
configSettingsYmlBS = $(embedFile configSettingsYml)

-- | @config/settings.yml@, parsed to a @Value@.
configSettingsYmlValue :: Value
configSettingsYmlValue =
  either Exception.throw id $
    decodeEither' configSettingsYmlBS
