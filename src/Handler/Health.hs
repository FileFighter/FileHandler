-- |
{-# LANGUAGE OverloadedStrings #-}

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

getHealthR :: Handler Value
getHealthR = do
  deploymentType <- liftIO getDeploymentType
  files <- liftIO $ concat <$> (mapM listDirectoryRelative =<< (filterM doesDirectoryExist =<< listDirectory "."))
  actualFilesSize <- liftIO $ sum <$> mapM getFileSize files
  let response =
        object
          [ "version" .= ("0.2.1" :: String),
            "deploymentType" .= deploymentType,
            "actualFilesSize" .= actualFilesSize,
            "fileCount" .= length files
          ]
  return response


getDeploymentType :: IO String
getDeploymentType = head . tail <$> getArgs

listDirectoryRelative :: FilePath -> IO [FilePath]
listDirectoryRelative x = Prelude.map (x </>) <$> listDirectory x
