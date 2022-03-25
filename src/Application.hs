{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns #-}

module Application where

import ClassyPrelude
    ( ($),
      Monad(return),
      Num((*)),
      Bool(False, True),
      Maybe(Just, Nothing),
      IO,
      const )
import Crypto.KeyEncrptionKey (createKeyEncrptionKey, getOrCreateKekIV)
import Data.Yaml.Config ( loadYamlSettingsArgs, useEnv )
import FileSystemServiceClient.FileSystemServiceClient (makeFileSystemServiceClient)
import Foundation
    ( Route(ErrorR, HomeR, DownloadR, UploadR, DeleteR, PreviewR,
            HealthR),
      App(..),
      resourcesApp )
import Handler.Delete ( deleteDeleteR )
import Handler.Download ( getDownloadR )
import Handler.Error ( getErrorR )
import Handler.Health ( getHealthR )
import Handler.Home ( getHomeR )
import Handler.Preview ( getPreviewR )
import Handler.Upload ( postUploadR )
import Network.Wai ()
import Network.Wai.Handler.Warp ( run )
import Network.Wai.Middleware.Cors
    ( cors,
      CorsResourcePolicy(CorsResourcePolicy, corsOrigins, corsMethods,
                         corsRequestHeaders, corsExposedHeaders, corsMaxAge, corsVaryOrigin,
                         corsRequireOrigin, corsIgnoreFailures) )
import Network.Wai.Parse ()
import Network.Wai.Middleware.Cors ()
import Settings
  ( AppSettings (encryptionPassword, fileSystemServiceSettings),
    configSettingsYmlValue,
  )
import Yesod.Core ( toWaiApp, mkYesodDispatch )

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

  application <- toWaiApp app

  run 5000 $ cors (const devCorsPolicy) application

devCorsPolicy =
  Just
    CorsResourcePolicy
      { corsOrigins = Just (["http://localhost:3000"],True),
        corsMethods = ["GET", "POST", "DELETE"],
        corsRequestHeaders = ["Authorization", "content-type", "X-FF-IDS", "X-FF-ID", "X-FF-NAME", "X-FF-PATH", "X-FF-SIZE","X-FF-PARENT-PATH","X-FF-RELATIVE-PATH","X-FF-PARENT-PATH"],
        corsExposedHeaders = Just ["Content-Disposition"],
        corsMaxAge = Just $ 60 * 60 * 24, -- one day
        corsVaryOrigin = False,
        corsRequireOrigin = False,
        corsIgnoreFailures = False
      }
