{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Handler.Download where

import ClassyPrelude
  ( Bool (True),
    Either (Right),
    FilePath,
    IO,
    Int,
    IsString (fromString),
    Maybe (..),
    Monad (return),
    MonadIO (..),
    Show (show),
    String,
    UTCTime,
    Utf8 (decodeUtf8),
    defaultTimeLocale,
    fromMaybe,
    maybe,
    pack,
    parseTimeM,
    unpack,
    void,
    ($),
    (++),
    (<$>),
  )
import ClassyPrelude.Yesod
  ( ConduitM,
    MonadHandler,
    MonadResource,
    TypedContent,
    addHeader,
    awaitForever,
    getYesod,
    invalidArgs,
    lookupGetParam,
    respondSource,
    runConduitRes,
    sendChunkBS,
    sendFile,
    sinkFile,
    yield,
    (.|),
  )
import qualified Data.ByteString.Char8 as S8
import FileStorage (getInodeModifcationTime, retrieveFile)
import FileSystemServiceClient.FileSystemServiceClient
  ( FileSystemServiceClient
      ( FileSystemServiceClient,
        getInodeContent
      ),
  )
import Foundation (App (App, fileSystemServiceClient), Handler)
import Models.Inode
  ( Inode (lastUpdated, mimeType, name, path, size),
  )
import qualified Network.HTTP.Types as HttpTypes
import System.Directory (doesDirectoryExist, removeFile)
import System.IO.Temp (emptySystemTempFile)
import UnliftIO.Resource (allocate)
import Utils.HandlerUtils (handleApiCall, lookupAuth)
import Utils.ZipFile

getDownloadR :: Handler TypedContent
getDownloadR = do
  App {fileSystemServiceClient = FileSystemServiceClient {getInodeContent = getInodeContent}} <- getYesod
  bearerToken <- lookupAuth

  inodeIds <- lookupRequiredInodeIds
  (responseBody, responseStatusCode, responseStatusMessage, maybeFilename) <- liftIO $ getInodeContent bearerToken inodeIds
  inodes <- handleApiCall responseBody responseStatusCode responseStatusMessage
  case inodes of
    [singleInode] -> do
      addHeader "Content-Disposition" $ pack ("attachment; filename=\"" ++ Models.Inode.name singleInode ++ "\"")
      respondSource (S8.pack $ fromMaybe "application/octet-stream" (mimeType singleInode)) $
        retrieveFile singleInode .| awaitForever sendChunkBS
    multipleInodes -> do
      let archiveName = fromMaybe "Files" maybeFilename
      addHeader "Content-Disposition" ("attachment; filename=\"" ++ decodeUtf8 archiveName ++ ".zip" ++ "\"")
      (_, tempFile) <- allocate (makeAllocateResource multipleInodes) freeResource
      sendFile "application/zip" tempFile

makeAllocateResource :: [Models.Inode.Inode] -> IO FilePath
makeAllocateResource inodes = do
  path <- emptySystemTempFile "FileFighterFileHandler.zip"
  createZip inodes path
  return path

freeResource :: FilePath -> IO ()
freeResource = removeFile


lookupRequiredInodeIds :: MonadHandler m => m String
lookupRequiredInodeIds = do
  maybeIds <- lookupGetParam "ids"
  maybe (invalidArgs ["Missing ids query parameter."]) return $ unpack <$> maybeIds
