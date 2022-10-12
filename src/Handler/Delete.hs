{-# LANGUAGE OverloadedStrings #-}

-- |
module Handler.Delete where

import ClassyPrelude hiding (Handler, filter)
import Data.Aeson
import Data.Maybe (fromMaybe)
import qualified Data.Text as DataText
import FileStorage (deleteFile, filterFiles, getPathFromFileId)
import FileSystemServiceClient.FileSystemServiceClient
import Foundation
import KeyStorage (deleteEncKey, maybeDeleteKeys)
import Models.Inode
import Network.HTTP.Req
import Network.HTTP.Types
import System.Directory
import Utils.HandlerUtils
import Yesod.Core
import Yesod.Persist (YesodPersist (runDB))
import Prelude (filter)

deleteDeleteR :: [Text] -> Handler Value
deleteDeleteR path = do
  App {fileSystemServiceClient = FileSystemServiceClient {deleteInode = deleteInode}, keyEncrptionKey = kek} <- getYesod
  authToken <- lookupAuth
  (responseBody, responseStatusCode, responseStatusMessage) <- liftIO $ deleteInode authToken path
  inodes <- handleApiCall responseBody responseStatusCode responseStatusMessage
  mapM_ deleteFile (filter filterFiles inodes) -- Todo: check if file exists
  maybeDeleteKeys kek inodes
  return responseBody
