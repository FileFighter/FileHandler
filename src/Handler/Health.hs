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
    Maybe,
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
import Data.Version (showVersion)
import Foundation
import KeyStorage (maybeCountKeys)
import qualified Network.HTTP.Types as HttpTypes
import Paths_FileHandlerYesod ()
import qualified Paths_FileHandlerYesod as BuildInfo
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
    fileCount :: Int,
    keyCount :: Int
  }
  deriving (Show, Generic)

instance ToJSON HealthInfo

getHealthR :: Handler Value
getHealthR = do
  App {appSettings = AppSettings {appProfile = deploymentType}, keyEncrptionKey = kek} <- getYesod
  files <- liftIO $ concat <$> (mapM listDirectoryRelative =<< (filterM doesDirectoryExist =<< listDirectory "."))
  actualFilesSize <- liftIO $ sum <$> mapM getFileSize files
  keyCount <- maybeCountKeys kek
  let response =
        HealthInfo
          { version = showVersion BuildInfo.version,
            deploymentType = deploymentType,
            actualFilesSize = actualFilesSize,
            fileCount = length files,
            keyCount = keyCount
          }
  returnJson response

listDirectoryRelative :: FilePath -> IO [FilePath]
listDirectoryRelative x = map (x </>) <$> listDirectory x
