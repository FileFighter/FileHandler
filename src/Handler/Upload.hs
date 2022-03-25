{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-deprecations #-}

-- |
module Handler.Upload where

import ClassyPrelude hiding (Handler)
import ClassyPrelude.Yesod
  ( ConduitT,
    FileInfo (fileContentType),
    runConduitRes,
    (.|), defaultMakeLogger, Response (responseBody)
  )
import Crypto.Cipher.AES
import Crypto.Cipher.Types (BlockCipher, IV, cipherInit, makeIV)
import Crypto.CryptoConduit (encryptConduit)
import Crypto.Error
import Crypto.KeyEncrptionKey hiding (initCipher, initIV)
import Crypto.Random
import Crypto.Types
import Data.Aeson
  ( Result (Error, Success),
    Value,
    fromJSON,
    object,
  )
import Data.ByteArray hiding (take)
import qualified Data.ByteString.Char8 as S8
import Data.CaseInsensitive (mk)
import qualified Data.Text as Text
import FileStorage (filterFiles, getPathFromFileId, storeFile)
import FileSystemServiceClient.FileSystemServiceClient
  ( FileSystemServiceClient (FileSystemServiceClient, createInode),
    UploadedInode (UploadedInode),
  )
import Foundation (App (App, fileSystemServiceClient, keyEncrptionKey), Handler)
import Models.Inode (Inode (fileSystemId))
import Network.HTTP.Types (Status (Status))
import UnliftIO.Resource
import Utils.HandlerUtils
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
import Crypto.Init
import System.Directory (createDirectoryIfMissing)
import Yesod.Core.Types (loggerPutStr)
import Prelude (read)
import Models.Path (Path(Path))

postUploadR :: Handler Value
postUploadR = do
  App {fileSystemServiceClient = FileSystemServiceClient {createInode = createInode}, keyEncrptionKey = kek} <- getYesod
  authToken <- lookupAuth
  (_params, files) <- runRequestBody
  case lookupSingleFile files of
    Nothing -> invalidArgs ["Missing required File."]
    Just file -> do
      inodeToCreate <- lookupUploadedInode $ Just (Text.unpack $ fileContentType file)
      case inodeToCreate of
        Nothing -> invalidArgs ["Missing required Header."]
        Just inode -> do
          (responseBody, responseStatusCode, responseStatusMessage) <- liftIO $ createInode authToken inode
          liftIO $ print $ show responseBody
          createdInodes <- handleApiCall responseBody responseStatusCode responseStatusMessage
          case filter filterFiles createdInodes of
            [singleInode] -> do
              let alloc = makeAllocateResource kek singleInode
              (_, _) <- allocate alloc (makeFreeResource file singleInode)
              return responseBody
            _ -> sendInternalError

lookupUploadedInode :: MonadHandler m => Maybe String -> m (Maybe UploadedInode)
lookupUploadedInode mimeType = do
  relativePath <- lookupHeader $ Data.CaseInsensitive.mk "X-FF-RELATIVE-PATH"
  parentPath <- lookupHeader $ Data.CaseInsensitive.mk "X-FF-PARENT-PATH"
  size <- lookupHeader $ Data.CaseInsensitive.mk "X-FF-SIZE"
  return $ UploadedInode <$> (Path . S8.unpack <$> parentPath) <*> (Path . S8.unpack <$> relativePath) <*> (read . S8.unpack <$> size) <*> mimeType

lookupSingleFile :: [(Text.Text, FileInfo)] -> Maybe FileInfo
lookupSingleFile [("file", file)] = Just file
lookupSingleFile _ = Nothing

-- this creates the encryptionKey by generating it
makeAllocateResource :: KeyEncryptionKey -> Inode -> IO (AES256, IV AES256)
makeAllocateResource kek inode = do
  secretKey :: Key AES256 ByteString <- genSecretKey (undefined :: AES256) 32
  let Key keyBytes = secretKey
  ivBytes <- genRandomIV (undefined :: AES256)
  createDirectoryIfMissing True $ "keys/" <> take 1 (show $ fileSystemId inode )
  writeFile ("keys/" <> getPathFromFileId (show $ fileSystemId inode) ++ ".key") (encryptWithKek kek keyBytes)
  writeFile ("keys/" <> getPathFromFileId (show $ fileSystemId inode) ++ ".iv") ivBytes

  return (initCipher secretKey, initIV ivBytes)

-- this takes the encryption information and encrypts and moves the file after the response has been send
makeFreeResource :: FileInfo -> Inode -> (AES256, IV AES256) -> IO ()
makeFreeResource fileInfo inode (cipher, iv) = do
  fileDest <- storeFile inode
  runConduitRes $
    fileSource fileInfo
      .| encryptConduit cipher iv mempty
      .| fileDest

