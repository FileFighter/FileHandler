-- |
{-# LANGUAGE OverloadedStrings #-}
module Handler.Delete where
import Foundation
import Yesod.Core


import ClassyPrelude hiding (filter, Handler)
import qualified Data.Text as DataText
import Data.Aeson
import Data.Maybe (fromMaybe)
import Models.Inode
import Network.HTTP.Req
import System.Directory
import FileSystemServiceClient.FileSystemServiceClient
import Network.HTTP.Types
import Utils.HandlerUtils
import FileStorage (filterFiles, getPathFromFileId)
import Prelude (filter)



serverPort = port 80

deleteDeleteR :: Int -> Handler Value
deleteDeleteR  inodeId = do
  App{fileSystemServiceClient = FileSystemServiceClient{deleteInode= deleteInode}} <- getYesod
  authToken  <- lookupBearerAuth
  case authToken of
    Nothing -> notAuthenticated
    Just bearerToken -> do
      (responseBody, responseStatusCode, responseStatusMessage) <- liftIO $ deleteInode  bearerToken (show inodeId)
      case responseStatusCode of
        200 -> do
          case fromJSON responseBody of
            Success inodes ->  do
              liftIO $ mapM_ deleteFile (filter filterFiles inodes) -- Todo: check if file exists
              return responseBody
            Error _ -> sendInternalError
        _ -> sendResponseStatus (Status responseStatusCode responseStatusMessage) responseBody



deleteFile :: Inode -> IO ()
deleteFile file = removeFile $ getPathFromFileId (show $ fileSystemId file)
