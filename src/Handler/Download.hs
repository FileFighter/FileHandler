{-# HLINT ignore "Use join" #-}
{-# HLINT ignore "Redundant bracket" #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Handler.Download where

import ClassyPrelude
  ( Bool (True),
    ByteString,
    Either (Right),
    FilePath,
    Functor (fmap),
    IO,
    Int,
    IsMap (lookup),
    IsString (fromString),
    Maybe (..),
    Monad (return, (>>=)),
    MonadIO (..),
    Monoid (mempty),
    Show (show),
    String,
    Text,
    Traversable (mapM),
    UTCTime,
    Utf8 (decodeUtf8),
    concat,
    concatMap,
    defaultTimeLocale,
    fromMaybe,
    id,
    join,
    map,
    maybe,
    pack,
    parseTimeM,
    print,
    pure,
    putStrLn,
    readFile,
    tshow,
    unpack,
    void,
    ($),
    (++),
    (.),
    (<$>),
    (<>),
    (=<<),
  )
import ClassyPrelude.Yesod
  ( ConduitM,
    Entity (Entity),
    MonadHandler,
    MonadResource,
    PersistQueryRead (selectFirst),
    PersistUniqueRead (getBy),
    TypedContent,
    Value,
    YesodPersist (runDB),
    addHeader,
    awaitForever,
    getYesod,
    invalidArgs,
    lookupGetParam,
    respondSource,
    runConduitRes,
    selectKeys,
    sendChunkBS,
    sendFile,
    sinkFile,
    yield,
    (.|),
  )
import Crypto.Cipher.AES
import Crypto.Cipher.Types
import Crypto.CryptoConduit (decryptConduit)
import Crypto.Init
import Crypto.KeyEncrptionKey (KeyEncryptionKey, decryptWithKek, getKeyForInode)
import Crypto.Types (Key (Key))
import DBModels (EncKey (EncKey, encKeyCipherIv, encKeyCipherKey), EntityField (EncKeyFsId, EncKeyId))
import qualified Data.ByteString.Char8 as S8
import Data.Text (splitAt, splitOn)
import Database.Persist (PersistQueryRead (selectKeysRes), (==.))
import FileStorage (getInodeModifcationTime, getPathFromFileId, retrieveFile)
import FileSystemServiceClient.FileSystemServiceClient
  ( FileSystemServiceClient
      ( FileSystemServiceClient,
        getInodeContent
      ),
    UploadedInode (parentPath),
  )
import Foundation (App (App, fileSystemServiceClient, keyEncrptionKey), Handler)
import Models.Inode
  ( Inode (lastUpdated, mimeType, name, path, size),
    fileSystemId,
  )
import Models.Path (Path, fromMultiPiece)
import Network.HTTP.Req (responseStatusMessage)
import qualified Network.HTTP.Types as HttpTypes
import System.Directory (doesDirectoryExist, removeFile)
import System.IO.Temp (emptySystemTempFile)
import UnliftIO.Resource (allocate)
import Utils.HandlerUtils (handleApiCall, lookupAuth, sendInternalError)
import Utils.ZipFile
import Yesod.Routes.TH.Types (flatten)

getDownloadR :: [Text] -> Handler TypedContent
getDownloadR path = do
  App {fileSystemServiceClient = FileSystemServiceClient {getInodeContent = getInodeContent}, keyEncrptionKey = kek} <- getYesod
  bearerToken <- lookupAuth

  paths <- lookupPaths path

  apiResponses <-
    liftIO $
      mapM
        ( \path -> do
            (responseBody, responseStatusCode, responseStatusMessage) <- getInodeContent bearerToken path
            return (responseBody, responseStatusCode, responseStatusMessage)
        )
        paths

  inodes <-
    concat
      <$> mapM
        ( \(responseBody, responseStatusCode, responseStatusMessage) -> do
            handleApiCall responseBody responseStatusCode responseStatusMessage
        )
        apiResponses

  case inodes of
    [singleInode] -> do
      liftIO $ print $ size singleInode
      addHeader "Content-Disposition" $ pack ("attachment; filename=\"" ++ Models.Inode.name singleInode ++ "\"")
      addHeader "Content-Length" $ tshow $ size singleInode
      --(key, iv) <- liftIO $ getKeyForInode kek singleInode
      runDB (selectFirst ([EncKeyFsId ==. (fileSystemId singleInode)]) ([])) >>= \case
        Nothing -> sendInternalError
        Just (Entity _ encKey) -> do
          let key' = initCipher $ Key (decryptWithKek kek $ encKeyCipherKey encKey) :: AES256
          respondSource (S8.pack $ fromMaybe "application/octet-stream" (mimeType singleInode)) $
            retrieveFile singleInode
              .| decryptConduit (key') (initIV $ encKeyCipherIv encKey) mempty
              .| awaitForever sendChunkBS
    multipleInodes -> do
      let archiveName = fromMaybe "Files" Nothing
      addHeader "Content-Disposition" ("attachment; filename=\"" ++ decodeUtf8 archiveName ++ ".zip" ++ "\"")
      (_, tempFile) <- allocate (makeAllocateResource kek multipleInodes) freeResource
      sendFile "application/zip" tempFile

lookupPaths :: MonadHandler m => [Text] -> m [Path]
lookupPaths parentPath = do
  maybeChildenParam <- lookupGetParam "children"
  case splitOn "," <$> maybeChildenParam of
    Just inodeNames -> pure $ map (\name -> fromMultiPiece $ parentPath <> [name]) inodeNames
    Nothing -> pure [fromMultiPiece parentPath]

makeAllocateResource :: KeyEncryptionKey -> [Models.Inode.Inode] -> IO FilePath
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
