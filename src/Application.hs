{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns #-}

module Application where

import ClassyPrelude
  ( Bool (False, True),
    IO,
    Maybe (Just, Nothing),
    Monad (return),
    Num ((*)),
    const,
    ($),
  )
import ClassyPrelude.Yesod (PersistConfig (createPoolConfig))
import Crypto.KeyEncrptionKey (createKeyEncrptionKey, getOrCreateKekIV)
import Data.Yaml.Config (loadYamlSettingsArgs, useEnv)
import FileSystemServiceClient.FileSystemServiceClient (makeFileSystemServiceClient)
import Foundation
  ( App (..),
    Route
      ( DeleteR,
        DownloadR,
        ErrorR,
        HealthR,
        HomeR,
        PreviewR,
        UploadR
      ),
    resourcesApp,
  )
import Handler.Delete (deleteDeleteR)
import Handler.Download (getDownloadR)
import Handler.Error (getErrorR)
import Handler.Health (getHealthR)
import Handler.Home (getHomeR)
import Handler.Preview (getPreviewR)
import Handler.Upload (postUploadR)
import Network.Wai ()
import Network.Wai.Handler.Warp (run)
import Network.Wai.Middleware.Cors
  ( CorsResourcePolicy
      ( CorsResourcePolicy,
        corsExposedHeaders,
        corsIgnoreFailures,
        corsMaxAge,
        corsMethods,
        corsOrigins,
        corsRequestHeaders,
        corsRequireOrigin,
        corsVaryOrigin
      ),
    cors,
  )
import Network.Wai.Parse ()
import Settings
  ( AppSettings (appDatabaseConf, encryptionPassword, fileSystemServiceSettings),
    configSettingsYmlValue,
  )
import Yesod.Core (mkYesodDispatch, toWaiApp)

mkYesodDispatch "App" resourcesApp

makeFoundation :: AppSettings -> IO App
makeFoundation appSettings = do
  let fssC = makeFileSystemServiceClient (fileSystemServiceSettings appSettings)

  iv <- getOrCreateKekIV
  let keyEncrptionKey = createKeyEncrptionKey (encryptionPassword appSettings) iv
  appConnPool <- createPoolConfig $ appDatabaseConf appSettings

  return
    App
      { appSettings = appSettings,
        appConnPool = appConnPool,
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
      { corsOrigins = Just (["http://localhost:3000"], True),
        corsMethods = ["GET", "POST", "DELETE"],
        corsRequestHeaders = ["Authorization", "content-type", "X-FF-IDS", "X-FF-ID", "X-FF-NAME", "X-FF-PATH", "X-FF-SIZE", "X-FF-PARENT-PATH", "X-FF-RELATIVE-PATH", "X-FF-PARENT-PATH"],
        corsExposedHeaders = Just ["Content-Disposition"],
        corsMaxAge = Just $ 60 * 60 * 24, -- one day
        corsVaryOrigin = False,
        corsRequireOrigin = False,
        corsIgnoreFailures = False
      }
