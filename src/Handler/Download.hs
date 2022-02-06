{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TupleSections #-}

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
    (<$>)
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
import FileStorage (getInodeModifcationTime, retrieveFile, getPathFromFileId)
import FileSystemServiceClient.FileSystemServiceClient
  ( FileSystemServiceClient
      ( FileSystemServiceClient,
        getInodeContent
      ),
  )
import Foundation (App (App, fileSystemServiceClient, keyEncrptionKey), Handler)
import Models.Inode
  ( Inode (lastUpdated, mimeType, name, path, size), fileSystemId
  )
import qualified Network.HTTP.Types as HttpTypes
import System.Directory (doesDirectoryExist, removeFile)
import System.IO.Temp (emptySystemTempFile)
import UnliftIO.Resource (allocate)
import Utils.HandlerUtils (handleApiCall, lookupAuth)
import Utils.ZipFile
import Crypto.Cipher.AES
import Crypto.Cipher.Types
import Crypto.KeyEncrptionKey (KeyEncryptionKey, decryptWithKek)
import ClassyPrelude
    ( ($),
      Monad(return),
      Functor(fmap),
      Show(show),
      Traversable(mapM),
      Monoid(mempty),
      IO,
      String,
      MonadIO(liftIO),
      fromMaybe,
      maybe,
      FilePath,
      (<$>),
      (++),
      readFile,
      tshow,
      pack,
      unpack,
      Utf8(decodeUtf8) )
import Crypto.Init
import Crypto.Types (Key(Key))
import Crypto.CryptoConduit (decryptConduit)

getDownloadR :: Handler TypedContent
getDownloadR = do
  App {fileSystemServiceClient = FileSystemServiceClient {getInodeContent = getInodeContent}, keyEncrptionKey = kek} <- getYesod
  bearerToken <- lookupAuth

  inodeIds <- lookupRequiredInodeIds
  (responseBody, responseStatusCode, responseStatusMessage, maybeFilename) <- liftIO $ getInodeContent bearerToken inodeIds
  inodes <- handleApiCall responseBody responseStatusCode responseStatusMessage
  case inodes of
    [singleInode] -> do
      addHeader "Content-Disposition" $ pack ("attachment; filename=\"" ++ Models.Inode.name singleInode ++ "\"")
      addHeader "Content-Length" $ tshow $ size singleInode
      (key, iv) <- liftIO $ getKeyForInode kek singleInode
      respondSource (S8.pack $ fromMaybe "application/octet-stream" (mimeType singleInode)) $
        retrieveFile singleInode
        .| decryptConduit key iv mempty
        .| awaitForever sendChunkBS
    multipleInodes -> do
      let archiveName = fromMaybe "Files" maybeFilename
      addHeader "Content-Disposition" ("attachment; filename=\"" ++ decodeUtf8 archiveName ++ ".zip" ++ "\"")
      (_, tempFile) <- allocate (makeAllocateResource kek multipleInodes) freeResource
      sendFile "application/zip" tempFile

makeAllocateResource :: KeyEncryptionKey  -> [Models.Inode.Inode] -> IO FilePath
makeAllocateResource kek inodes = do
  path <- emptySystemTempFile "FileFighterFileHandler.zip"
  inodesWithKeys <- mapM (\inode -> fmap (inode,) (getKeyForInode kek inode)) inodes
  createZip inodesWithKeys path
  return path

freeResource :: FilePath -> IO ()
freeResource = removeFile


lookupRequiredInodeIds :: MonadHandler m => m String
lookupRequiredInodeIds = do
  maybeIds <- lookupGetParam "ids"
  maybe (invalidArgs ["Missing ids query parameter."]) return $ unpack <$> maybeIds



getKeyForInode ::  KeyEncryptionKey -> Inode ->  IO (AES256, IV AES256)
getKeyForInode kek inode = do
  key <- decryptWithKek kek <$> readFile (getPathFromFileId (show $ fileSystemId inode) ++ ".key")
  iv <- readFile (getPathFromFileId (show $ fileSystemId inode) ++ ".iv")

  return (initCipher $ Key key, initIV iv)
