{-# LANGUAGE NoImplicitPrelude #-}

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
      TypedContent )
import qualified Data.ByteString.Char8 as S8
import FileStorage (retrieveFile)
import Foundation
import Models.Inode


import Utils.HandlerUtils
import FileSystemServiceClient.FileSystemServiceClient hiding (mimeType)
import Crypto.KeyEncrptionKey
import ClassyPrelude
    ( ($),
      Show(show),
      Monoid(mempty),
      Int,
      fromMaybe,
      MonadIO(liftIO),
      String )
import Crypto.CryptoConduit

getPreviewR :: Int -> String -> Handler TypedContent
getPreviewR inodeId _ = do
  App {fileSystemServiceClient = FileSystemServiceClient {getInodeInfo = getInodeInfo'}, keyEncrptionKey = kek} <- getYesod
  bearerToken <- lookupAuth

  (responseBody', responseStatusCode, responseStatusMessage) <- liftIO $ getInodeInfo' bearerToken $ show inodeId
  inode <- handleApiCall responseBody' responseStatusCode responseStatusMessage
  (key, iv) <- liftIO $ getKeyForInode kek inode
  respondSource (S8.pack $ fromMaybe "application/octet-stream" (mimeType inode)) $
    retrieveFile inode
    .| decryptConduit key iv mempty
    .| awaitForever sendChunkBS
