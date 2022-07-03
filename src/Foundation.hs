{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}

module Foundation where

import ClassyPrelude hiding (Handler)
import ClassyPrelude.Yesod (YesodPersist (runDB), getYesod)
import Crypto.KeyEncrptionKey (KeyEncryptionKey)
import Database.Persist.MongoDB hiding (master)
import FileSystemServiceClient.FileSystemServiceClient
  ( FileSystemServiceClient,
  )
import Network.Wai.Parse
  ( tempFileBackEnd,
  )
import Settings (AppSettings (appDatabaseConf))
import Yesod.Core
  ( FileUpload (FileUploadDisk),
    RenderRoute (renderRoute),
    Yesod (fileUpload, maximumContentLength),
    mkYesodData,
    parseRoutesFile,
  )
import Yesod.Core.Types
import Yesod.Persist.Core (YesodPersist (..))

data App = App
  { appSettings :: AppSettings,
    appConnPool :: ConnectionPool,
    fileSystemServiceClient :: FileSystemServiceClient,
    keyEncrptionKey :: KeyEncryptionKey,
    appLogger :: Logger
  }

mkYesodData "App" $(parseRoutesFile "routes.yesodroutes")

instance Yesod App where
  maximumContentLength _ (Just UploadR) = Nothing
  maximumContentLength _ _ = Just (2 * 1024 * 1024) -- 2 megabytes
  fileUpload _ _ = FileUploadDisk tempFileBackEnd

-- How to run database actions.
instance YesodPersist App where
  type YesodPersistBackend App = MongoContext
  runDB :: ReaderT MongoContext Handler a -> Handler a
  runDB action = do
    master <- getYesod
    runMongoDBPool
      (mgAccessMode $ appDatabaseConf $ appSettings master)
      action
      (appConnPool master)
