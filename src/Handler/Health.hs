-- |
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Handler.Health where

import Foundation
import Yesod.Core
import qualified Network.HTTP.Types as HttpTypes
import Network.Wai
import Data.Aeson
import System.Environment
import System.Directory
import System.FilePath
import Control.Monad
import GHC.Generics
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
listDirectoryRelative x = Prelude.map (x </>) <$> listDirectory x
