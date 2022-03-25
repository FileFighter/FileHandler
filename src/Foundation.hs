{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}

module Foundation where

import ClassyPrelude
import FileSystemServiceClient.FileSystemServiceClient
  ( FileSystemServiceClient,
  )
import Network.Wai.Parse
  ( tempFileBackEnd,
  )
import Settings (AppSettings)
import Yesod.Core
  ( FileUpload (FileUploadDisk),
    RenderRoute (renderRoute),
    Yesod (fileUpload, maximumContentLength),
    mkYesodData,
    parseRoutesFile,
  )
import Crypto.KeyEncrptionKey (KeyEncryptionKey)

data App = App
  { appSettings :: AppSettings,
    fileSystemServiceClient :: FileSystemServiceClient,
    keyEncrptionKey ::  KeyEncryptionKey
  }

mkYesodData "App" $(parseRoutesFile "routes.yesodroutes")

instance Yesod App where
  maximumContentLength _ (Just (UploadR)) = Nothing
  maximumContentLength _ _ = Just (2 * 1024 * 1024) -- 2 megabytes
  fileUpload _ _ = FileUploadDisk tempFileBackEnd
