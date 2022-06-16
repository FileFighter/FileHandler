{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

-- |
module Handler.Health where

import ClassyPrelude
  ( FilePath,
    Generic,
    IO,
    Int,
    Integer,
    IsSequence (filterM),
    MonadIO (liftIO),
    Show,
    String,
    Traversable (mapM),
    concat,
    length,
    map,
    sum,
    ($),
    (<$>),
    (</>),
    (=<<),
  )
import Foundation
import qualified Network.HTTP.Types as HttpTypes
import Settings (AppSettings (AppSettings), appProfile)
import System.Directory
  ( doesDirectoryExist,
    getFileSize,
    listDirectory,
  )
import Yesod.Core

data HealthInfo = HealthInfo
  { version :: String,
    deploymentType :: String,
    actualFilesSize :: Integer,
    fileCount :: Int
  }
  deriving (Show, Generic)

instance ToJSON HealthInfo

getHealthR :: Handler Value
getHealthR = do
  App {appSettings = AppSettings {appProfile = deploymentType}} <- getYesod
  files <- liftIO $ concat <$> (mapM listDirectoryRelative =<< (filterM doesDirectoryExist =<< listDirectory "."))
  actualFilesSize <- liftIO $ sum <$> mapM getFileSize files
  let response =
        HealthInfo
          { version = "0.2.1" :: String,
            deploymentType = deploymentType,
            actualFilesSize = actualFilesSize,
            fileCount = length files
          }
  returnJson response

listDirectoryRelative :: FilePath -> IO [FilePath]
listDirectoryRelative x = map (x </>) <$> listDirectory x
