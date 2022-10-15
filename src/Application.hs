{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns #-}

module Application
  ( appMain,
    makeFoundation,
    makeLogWare,
  )
where

import ClassyPrelude
  ( Applicative ((<*>)),
    Bool (False, True),
    ByteString,
    Eq ((/=), (==)),
    Functor (fmap),
    IO,
    Maybe (Just, Nothing),
    Monad (return, (>>=)),
    Num ((*)),
    Semigroup ((<>)),
    Show (show),
    String,
    Text,
    const,
    isJust,
    map,
    when,
    ($),
    (<$>),
    (||),
  )
import ClassyPrelude.Yesod (Default (def), PersistConfig (createPoolConfig))
import Crypto.KeyEncrptionKey (createKeyEncrptionKey, getOrCreateKekIV)
import Data.ByteString (pack)
import Data.ByteString.UTF8 (fromString)
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
import Network.Wai (Middleware)
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
import Network.Wai.Middleware.RequestLogger (Destination (Logger), IPAddrSource (FromFallback, FromSocket), OutputFormat (Apache, Detailed), RequestLoggerSettings (destination, outputFormat), mkRequestLogger)
import Network.Wai.Parse ()
import Settings
  ( AppSettings (appDatabaseConf, appProfile, encryptionPassword, fileSystemServiceSettings, frontendOrigin),
    configSettingsYmlValue,
  )
import System.Log.FastLogger
  ( defaultBufSize,
    newStdoutLoggerSet,
    toLogStr,
  )
import Utils.FileFighterBanner (printBanner)
import Yesod.Core (mkYesodDispatch, toWaiApp)
import Yesod.Core.Types (Logger (loggerSet), loggerPutStr)
import Yesod.Default.Config2 (makeYesodLogger)

mkYesodDispatch "App" resourcesApp

makeFoundation :: AppSettings -> IO App
makeFoundation appSettings = do
  let fssC = makeFileSystemServiceClient (fileSystemServiceSettings appSettings)
  let maybeEncryptionPassword = case encryptionPassword appSettings of
        Just "null" -> Nothing
        Nothing -> Nothing
        Just password -> Just password

  iv <- if isJust maybeEncryptionPassword then getOrCreateKekIV else return "FallBackIV"
  let keyEncrptionKey = createKeyEncrptionKey <$> maybeEncryptionPassword <*> Just iv
  appConnPool <- createPoolConfig $ appDatabaseConf appSettings
  appLogger <- newStdoutLoggerSet defaultBufSize >>= makeYesodLogger
  printBanner $ loggerPutStr appLogger
  loggerPutStr appLogger $ toLogStr $ "Using Config: \n" <> show appSettings <> "\n"

  return
    App
      { appSettings = appSettings,
        appConnPool = appConnPool,
        fileSystemServiceClient = fssC,
        keyEncrptionKey = keyEncrptionKey,
        appLogger = appLogger
      }

makeLogWare :: App -> IO Middleware
makeLogWare foundation = do
  let profile = appProfile $ appSettings foundation
  let nonProd = "stage" == profile || "dev" == profile
  mkRequestLogger
    def
      { outputFormat =
          if nonProd
            then Detailed True
            else
              Apache
                ( if nonProd
                    then FromFallback
                    else FromSocket
                ),
        destination = Logger $ loggerSet $ appLogger foundation
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

  run 5000 $ cors (const $ corsPolicy $ frontendOrigin settings) application

corsPolicy :: String -> Maybe CorsResourcePolicy
corsPolicy frontendOrigin =
  Just
    CorsResourcePolicy
      { corsOrigins = Just ([fromString frontendOrigin], True),
        corsMethods = ["GET", "POST", "DELETE"],
        corsRequestHeaders = ["Authorization", "content-type", "X-FF-IDS", "X-FF-ID", "X-FF-NAME", "X-FF-PATH", "X-FF-SIZE", "X-FF-PARENT-PATH", "X-FF-RELATIVE-PATH", "X-FF-PARENT-PATH"],
        corsExposedHeaders = Just ["Content-Disposition"],
        corsMaxAge = Just $ 60 * 60 * 24, -- one day
        corsVaryOrigin = False,
        corsRequireOrigin = False,
        corsIgnoreFailures = False
      }
