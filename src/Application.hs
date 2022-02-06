{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns #-}

module Application where

import ClassyPrelude
import Crypto.KeyEncrptionKey (createKeyEncrptionKey, getOrCreateKekIV)
import Data.Yaml.Config
import FileSystemServiceClient.FileSystemServiceClient (makeFileSystemServiceClient)
import Foundation
import Handler.Delete
import Handler.Download
import Handler.Error
import Handler.Health
import Handler.Home
import Handler.Preview
import Handler.Upload
import Settings
  ( AppSettings (encryptionPassword, fileSystemServiceSettings),
    configSettingsYmlValue,
  )
import Yesod.Core

mkYesodDispatch "App" resourcesApp

makeFoundation :: AppSettings -> IO App
makeFoundation appSettings = do
  let fssC = makeFileSystemServiceClient (fileSystemServiceSettings appSettings)

  iv <- getOrCreateKekIV
  let keyEncrptionKey = createKeyEncrptionKey (encryptionPassword appSettings) iv

  return
    App
      { appSettings = appSettings,
        fileSystemServiceClient = fssC,
        keyEncrptionKey = keyEncrptionKey
      }

appMain :: IO ()
appMain = do
  -- Get the settings from all relevant sources
  settings <-
    loadYamlSettingsArgs
      -- fall back to compile-time values, set to [] to require values at runtime
      [configSettingsYmlValue]
      -- allow environment variables to override
      useEnv

  app <- makeFoundation settings

  warp 5000 app
