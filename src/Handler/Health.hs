{-# LANGUAGE OverloadedStrings #-}

-- |
module Handler.Health where

import Control.Monad
import Data.Aeson
import Foundation
import qualified Network.HTTP.Types as HttpTypes
import Network.Wai
import System.Directory
import System.Environment
import System.FilePath
import Yesod.Core

data HealthInfo = HealthInfo
  { version :: String,
    deploymentType :: String,
    actualFilesSize :: Integer,
    fileCount :: Int
  }

getHealthR :: Handler HealthInfo
getHealthR = do
  deploymentType <- liftIO getDeploymentType
  files <- liftIO $ concat <$> (mapM listDirectoryRelative =<< (filterM doesDirectoryExist =<< listDirectory "."))
  actualFilesSize <- liftIO $ sum <$> mapM getFileSize files
  let response =
        HealthInfo
          { version = "0.2.1" :: String,
            deploymentType = deploymentType,
            actualFilesSize = actualFilesSize,
            fileCount = length files
          }
  return response

getDeploymentType :: IO String
getDeploymentType = head . tail <$> getArgs

listDirectoryRelative :: FilePath -> IO [FilePath]
listDirectoryRelative x = Prelude.map (x </>) <$> listDirectory x
