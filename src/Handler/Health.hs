-- |
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Handler.Health where

import Foundation
import ClassyPrelude hiding (Handler)
import Yesod.Core
import qualified Network.HTTP.Types as HttpTypes
import System.Directory
    ( doesDirectoryExist, getFileSize, listDirectory )
import Settings (AppSettings(AppSettings), appProfile)


data HealthInfo =HealthInfo
  { version :: String
  , deploymentType :: String
  , actualFilesSize :: Integer
  , fileCount :: Int
  }
  deriving (Show, Generic)

instance ToJSON  HealthInfo

getHealthR :: Handler Value
getHealthR = do
  App{appSettings = AppSettings {appProfile = deploymentType}} <- getYesod
  files <- liftIO $ concat <$> (mapM listDirectoryRelative =<< (filterM doesDirectoryExist =<< listDirectory "."))
  actualFilesSize <- liftIO $ sum <$> mapM getFileSize files
  let response =
        HealthInfo
          { version = "0.2.1" :: String
          ,   deploymentType = deploymentType
          ,  actualFilesSize = actualFilesSize
          ,  fileCount = length files
          }
  returnJson response


listDirectoryRelative :: FilePath -> IO [FilePath]
listDirectoryRelative x = map (x </>) <$> listDirectory x
