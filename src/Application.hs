{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns #-}

module Application where

import ClassyPrelude
import Data.Yaml.Config
import FileSystemServiceClient.FileSystemServiceClient (makeFileSystemServiceClient)
import Foundation
import Handler.Delete
import Handler.Download
import Handler.Health
import Handler.Home
import Handler.Preview
import Handler.Upload
import Handler.Error
import Settings
import Yesod.Core

mkYesodDispatch "App" resourcesApp

makeFoundation :: AppSettings -> IO App
makeFoundation appSettings = do
  let fssC = makeFileSystemServiceClient (fileSystemServiceSettings appSettings)
  return
    App
      { appSettings = appSettings,
        fileSystemServiceClient = fssC
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
