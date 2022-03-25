{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Handler.Preview where

import ClassyPrelude.Yesod
    ( ($),
      Show(show),
      Int,
      getYesod,
      (.|),
      MonadIO(liftIO),
      String,
      fromMaybe,
      awaitForever,
      respondSource,
      sendChunkBS,
      TypedContent, badRequest400, status400, ToJSON (toJSON) )
import qualified Data.ByteString.Char8 as S8
import FileStorage (retrieveFile, filterFiles)
import Foundation
import Models.Inode


import Utils.HandlerUtils
    ( lookupAuth, handleApiCall, sendErrorOrRedirect )
import FileSystemServiceClient.FileSystemServiceClient hiding (mimeType)
import Crypto.KeyEncrptionKey ( getKeyForInode )
import ClassyPrelude
    ( ($),
      Show(show),
      Monoid(mempty),
      Int,
      fromMaybe,
      MonadIO(liftIO),
      String,
      Text,
      (.),
      print,
      Bool(True),
      intercalate,
      (<>),
      map )
import Crypto.CryptoConduit
import FileSystemServiceClient.FileSystemServiceClient (FileSystemServiceClient(getInodeContent))
import Models.RestApiStatus (RestApiStatus(RestApiStatus))
import Models.Path (fromMultiPiece)

getPreviewR ::  [Text] -> Handler TypedContent
getPreviewR path = do
  App {fileSystemServiceClient = FileSystemServiceClient {getInodeContent = getInodeContent'}, keyEncrptionKey = kek} <- getYesod
  bearerToken <- lookupAuth

  (responseBody', responseStatusCode, responseStatusMessage) <- liftIO $ getInodeContent' bearerToken $ fromMultiPiece path
  inodes <- handleApiCall responseBody' responseStatusCode responseStatusMessage
  case map (\i -> (i,filterFiles i))inodes of
    [(inode,True)] -> do
      (key, iv) <- liftIO $ getKeyForInode kek inode
      respondSource (S8.pack $ fromMaybe "application/octet-stream" (mimeType inode)) $
        retrieveFile inode
        .| decryptConduit key iv mempty
        .| awaitForever sendChunkBS
    _ -> sendErrorOrRedirect status400 $ toJSON $ RestApiStatus  "Can not preview a folder." "Bad Request"
