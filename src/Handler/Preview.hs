{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Handler.Preview where

import ClassyPrelude
  ( Bool (True),
    Int,
    MonadIO (liftIO),
    Monoid (mempty),
    Show (show),
    String,
    Text,
    fromMaybe,
    intercalate,
    map,
    print,
    ($),
    (.),
    (<>),
  )
import ClassyPrelude.Yesod
  ( Int,
    MonadIO (liftIO),
    Show (show),
    String,
    ToJSON (toJSON),
    TypedContent,
    YesodPersist (runDB),
    awaitForever,
    badRequest400,
    fromMaybe,
    getYesod,
    respondSource,
    sendChunkBS,
    status400,
    ($),
    (.|),
  )
import Crypto.CryptoConduit
import Crypto.KeyEncrptionKey (getKeyForInode)
import qualified Data.ByteString.Char8 as S8
import FileStorage (filterFiles, retrieveFile)
import FileSystemServiceClient.FileSystemServiceClient (FileSystemServiceClient (getInodeContent))
import FileSystemServiceClient.FileSystemServiceClient hiding (mimeType)
import Foundation
import KeyStorage (getDecryptionFunctionMaybeFromDB, getEncKeyOrInternalError)
import Models.Inode
import Models.Path (fromMultiPiece)
import Models.RestApiStatus (RestApiStatus (RestApiStatus))
import Utils.HandlerUtils
  ( handleApiCall,
    lookupAuth,
    sendErrorOrRedirect,
  )

getPreviewR :: [Text] -> Handler TypedContent
getPreviewR path = do
  App {fileSystemServiceClient = FileSystemServiceClient {getInodeContent = getInodeContent'}, keyEncrptionKey = kek} <- getYesod
  bearerToken <- lookupAuth
  (responseBody', responseStatusCode, responseStatusMessage) <- liftIO $ getInodeContent' bearerToken $ fromMultiPiece path
  inodes <- handleApiCall responseBody' responseStatusCode responseStatusMessage
  case map (\i -> (i, filterFiles i)) inodes of
    [(inode, True)] -> do
      (inode, decryptFunc) <- getDecryptionFunctionMaybeFromDB inode kek
      respondSource (S8.pack $ fromMaybe "application/octet-stream" (mimeType inode)) $
        retrieveFile inode
          .| decryptFunc
          .| awaitForever sendChunkBS
    _ -> sendErrorOrRedirect status400 $ toJSON $ RestApiStatus "Can not preview a folder." "Bad Request"
