{-# LANGUAGE GADTs #-}
{-# HLINT ignore "Use join" #-}
{-# HLINT ignore "Redundant bracket" #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Unused LANGUAGE pragma" #-}

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
    const,
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
    zip,
    zipWith,
    ($),
    (++),
    (.),
    (<$>),
    (<>),
    (=<<),
  )
import ClassyPrelude.Yesod
  ( ConduitM,
    ConduitT,
    Entity (Entity),
    MonadHandler,
    MonadResource,
    PersistQueryRead (selectFirst),
    PersistUniqueRead (getBy),
    ResourceT,
    ToJSON (toJSON),
    TypedContent,
    Value,
    YesodPersist (runDB),
    addHeader,
    awaitForever,
    getYesod,
    invalidArgs,
    lookupGetParam,
    respondSource,
    runConduit,
    runConduitRes,
    selectKeys,
    sendChunkBS,
    sendFile,
    sinkFile,
    status400,
    takeWhileCE,
    yield,
    (.|),
  )
import Crypto.Cipher.AES
import Crypto.Cipher.Types
import Crypto.CryptoConduit (decryptConduit)
import Crypto.Init
import Crypto.KeyEncrptionKey (KeyEncryptionKey, decryptWithKek, getKeyForInode)
import Crypto.Types (Key (Key))
import DBModels (EncKey (EncKey, encKeyCipherIv, encKeyCipherKey))
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
import KeyStorage (getDecryptionFunctionMaybeFromDB, getEncKeyOrInternalError)
import Models.Inode
  ( Inode (lastUpdated, mimeType, name, path, size),
    fileSystemId,
    getFirstPathPiece,
  )
import Models.Path (Path, fromMultiPiece)
import Models.RestApiStatus (RestApiStatus (RestApiStatus))
import Network.HTTP.Req (responseStatusMessage)
import qualified Network.HTTP.Types as HttpTypes
import System.Directory (doesDirectoryExist, removeFile)
import System.IO.Temp (emptySystemTempFile)
import UnliftIO.Resource (allocate, runResourceT)
import Utils.HandlerUtils (handleApiCall, handleApiCall', lookupAuth, sendErrorOrRedirect, sendInternalError)
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
    concat <$> mapM handleApiCall' apiResponses

  case inodes of
    [] -> sendErrorOrRedirect status400 $ toJSON $ RestApiStatus "Can not download a empty folder." "Bad Request"
    [singleInode] -> do
      liftIO $ print $ size singleInode
      (inode, decFunc) <- getDecryptionFunctionMaybeFromDB singleInode kek

      addHeader "Content-Disposition" $ pack ("attachment; filename=\"" ++ Models.Inode.name singleInode ++ "\"")
      addHeader "Content-Length" $ tshow $ size singleInode
      respondSource (S8.pack $ fromMaybe "application/octet-stream" (mimeType singleInode)) $
        retrieveFile singleInode
          .| decFunc
          .| awaitForever sendChunkBS
    first : moreInodes -> do
      let archiveName = getFirstPathPiece first
      addHeader "Content-Disposition" ("attachment; filename=\"" ++ pack archiveName ++ ".zip" ++ "\"")
      encKeysWithInodes <- mapM (`getDecryptionFunctionMaybeFromDB` kek) (first : moreInodes)
      path <- liftIO $ emptySystemTempFile "FileFighterFileHandler.zip"
      runConduit $ createZip encKeysWithInodes path
      (_, tempFile) <- allocate (makeAllocateResource path) freeResource
      sendFile "application/zip" tempFile

justOrInternalError :: MonadHandler m => Maybe a -> m a
justOrInternalError (Just a) = return a
justOrInternalError Nothing = sendInternalError

lookupPaths :: MonadHandler m => [Text] -> m [Path]
lookupPaths parentPath = do
  maybeChildenParam <- lookupGetParam "children"
  case splitOn "," <$> maybeChildenParam of
    Just inodeNames -> pure $ map (\name -> fromMultiPiece $ parentPath <> [name]) inodeNames
    Nothing -> pure [fromMultiPiece parentPath]

makeAllocateResource :: FilePath -> IO FilePath
makeAllocateResource = return

freeResource :: FilePath -> IO ()
freeResource = removeFile

lookupRequiredInodeIds :: MonadHandler m => m String
lookupRequiredInodeIds = do
  maybeIds <- lookupGetParam "ids"
  maybe (invalidArgs ["Missing ids query parameter."]) return $ unpack <$> maybeIds
