{-# LANGUAGE OverloadedStrings #-}

-- |
module Handler.Delete where

import ClassyPrelude hiding (Handler, filter)
import Data.Aeson
import Data.Maybe (fromMaybe)
import qualified Data.Text as DataText
import FileStorage (filterFiles, getPathFromFileId)
import FileSystemServiceClient.FileSystemServiceClient
import Foundation
import Models.Inode
import Network.HTTP.Req
import Network.HTTP.Types
import System.Directory
import Utils.HandlerUtils
import Yesod.Core
import Prelude (filter)

deleteDeleteR :: [Text] -> Handler Value
deleteDeleteR path = do
  App {fileSystemServiceClient = FileSystemServiceClient {deleteInode = deleteInode}} <- getYesod
  authToken <- lookupAuth
  (responseBody, responseStatusCode, responseStatusMessage) <- liftIO $ deleteInode authToken path
  inodes <- handleApiCall responseBody responseStatusCode responseStatusMessage
  liftIO $ mapM_ deleteFile (filter filterFiles inodes) -- Todo: check if file exists
  return responseBody

deleteFile :: Inode -> IO ()
deleteFile file = removeFile $ getPathFromFileId (show $ fileSystemId file)
