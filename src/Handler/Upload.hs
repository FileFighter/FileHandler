{-# LANGUAGE OverloadedStrings #-}

-- |
module Handler.Upload where

import ClassyPrelude hiding (Handler)
import ClassyPrelude.Yesod
  ( FileInfo (fileContentType),
    runConduitRes,
    (.|),
  )
import Data.Aeson
  ( Result (Error, Success),
    Value,
    fromJSON,
    object,
  )
import qualified Data.ByteString.Char8 as S8
import Data.CaseInsensitive (mk)
import qualified Data.Text as Text
import FileStorage (storeFile,filterFiles)
import FileSystemServiceClient.FileSystemServiceClient
  ( FileSystemServiceClient (FileSystemServiceClient, createInode),
    UploadedInode (UploadedInode),
  )
import Foundation (App (App, fileSystemServiceClient), Handler)
import Models.Inode (Inode (fileSystemId))
import Network.HTTP.Types (Status (Status))
import Yesod.Core
  ( FileInfo,
    MonadHandler,
    MonadIO (liftIO),
    fileSource,
    getYesod,
    invalidArgs,
    lookupBearerAuth,
    lookupHeader,
    notAuthenticated,
    runRequestBody,
    sendResponseStatus,
  )
import Yesod.Core.Handler (sendResponseCreated)
import Utils.HandlerUtils

postUploadR :: Int -> Handler Value
postUploadR parentId = do
  App {fileSystemServiceClient = FileSystemServiceClient {createInode = createInode}} <- getYesod
  authToken <- lookupBearerAuth
  case authToken of
    Nothing -> notAuthenticated
    Just bearerToken -> do
      (_params, files) <- runRequestBody
      case lookupSingleFile files of
        Nothing -> invalidArgs ["Missing required File."]
        Just file -> do
          inodeToCreate <- lookupUploadedInode $ Just (Text.unpack $ fileContentType file)
          case inodeToCreate of
            Nothing -> invalidArgs ["Missing required Header."]
            Just inode -> do
              (responseBody, responseStatusCode, responseStatusMessage) <- liftIO $ createInode bearerToken inode (show parentId)
              case responseStatusCode of
                201 -> do
                  case fromJSON responseBody of
                    Success createdInodes -> do
                      case filter filterFiles createdInodes of
                        [singleInode] -> do
                          let a = fileSystemId singleInode
                          fileDest <- liftIO $ storeFile singleInode
                          runConduitRes $ fileSource file .| fileDest
                          return responseBody
                        _ -> sendInternalError
                    Error _ -> sendInternalError
                _ -> sendResponseStatus (Status responseStatusCode responseStatusMessage) responseBody

lookupUploadedInode :: MonadHandler m => Maybe String -> m (Maybe UploadedInode)
lookupUploadedInode mimeType = do
  name <- lookupHeader $ Data.CaseInsensitive.mk "X-FF-NAME"
  path <- lookupHeader $ Data.CaseInsensitive.mk "X-FF-PATH"
  size <- lookupHeader $ Data.CaseInsensitive.mk "X-FF-SIZE"

  return $ UploadedInode <$> (S8.unpack <$> name) <*> (S8.unpack <$> path) <*> mimeType <*> (S8.unpack <$> size)

lookupSingleFile :: [(Text.Text, FileInfo)] -> Maybe FileInfo
lookupSingleFile [("file", file)] = Just file
lookupSingleFile _ = Nothing
