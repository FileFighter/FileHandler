{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-deferred-out-of-scope-variables #-}
{-# OPTIONS_GHC -Wno-deprecations #-}

-- |
module Handler.Upload where

import ClassyPrelude
  ( Applicative ((<*>)),
    ByteString,
    Eq ((/=)),
    IO,
    Integer,
    IsSequence (filter),
    Maybe (..),
    Monad (return, (>>=)),
    MonadIO (liftIO),
    Monoid (mempty),
    Show (show),
    Text,
    print,
    singleton,
    undefined,
    ($),
    (.),
    (<$>),
  )
import ClassyPrelude.Yesod
  ( ConduitT,
    FileInfo (fileContentType),
    MonadHandler (HandlerSite),
    PersistStoreWrite (insertKey),
    RedirectUrl,
    RenderRoute (Route),
    Response (responseBody),
    defaultMakeLogger,
    lengthC,
    lengthCE,
    runConduitRes,
    (.|),
  )
import Crypto.Cipher.AES
import Crypto.Cipher.Types (BlockCipher, IV, cipherInit, makeIV)
import Crypto.CryptoConduit (encryptConduit)
import Crypto.Error
import Crypto.Init
import Crypto.KeyEncrptionKey hiding (initCipher, initIV)
import Crypto.RandomGen
import Crypto.Types
import DBModels (EncKey (EncKey), EntityField (EncKeyId), Key (EncKeyKey))
import Data.Aeson
  ( Result (Error, Success),
    Value,
    fromJSON,
    object,
  )
import Data.ByteArray hiding (pack, take)
import qualified Data.ByteString.Char8 as S8
import Data.CaseInsensitive (mk)
import qualified Data.Text as Text
import FileStorage (filterFiles, getPathFromFileId, storeFile)
import FileSystemServiceClient.FileSystemServiceClient
  ( FileSystemServiceClient (FileSystemServiceClient, createInode, preflightInode),
    PreflightInode (PreflightInode),
    UploadedInode (UploadedInode),
  )
import Foundation (App (App, fileSystemServiceClient, keyEncrptionKey), Handler)
import KeyStorage (getEncKeyOrInternalError, storeEncKey)
import Models.Inode (Inode (fileSystemId))
import Models.Path (Path (Path))
import Network.HTTP.Types (Status (Status))
import System.Directory (createDirectoryIfMissing, doesDirectoryExist)
import UnliftIO.Resource
import Utils.HandlerUtils
import Yesod (YesodPersist (runDB))
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
import Yesod.Core.Types (FileInfo (fileSourceRaw), loggerPutStr)
import Prelude (read)

postUploadR :: Handler Value
postUploadR = do
  App {fileSystemServiceClient = fssc, keyEncrptionKey = kek} <- getYesod
  let FileSystemServiceClient {createInode = createInode} = fssc
  authToken <- lookupAuth
  performPreflight fssc authToken
  (_params, files) <- runRequestBody
  case lookupSingleFile files of
    Nothing -> invalidArgs ["Missing required File."]
    Just file -> do
      lookupUploadedInode file >>= \case
        Nothing -> invalidArgs ["Missing required Header."]
        Just inode -> do
          (responseBody, responseStatusCode, responseStatusMessage) <- liftIO $ createInode authToken inode
          liftIO $ print $ show responseBody
          createdInodes <- handleApiCall responseBody responseStatusCode responseStatusMessage
          case filter filterFiles createdInodes of
            [singleInode] -> do
              (alloc, encKey') <- liftIO $ makeAllocateResource kek singleInode
              runDB $ storeEncKey singleInode encKey'
              (_, _) <- allocate alloc (makeFreeResource file singleInode)
              return responseBody
            _ -> sendInternalError

performPreflight :: (MonadHandler m, RedirectUrl (HandlerSite m) (Route App, [(Text, Text)])) => FileSystemServiceClient -> Text -> m ()
performPreflight FileSystemServiceClient {preflightInode = _preflightInode} authToken = do
  lookupPreflightInode >>= \case
    Nothing -> invalidArgs ["Missing required Header: Need X-FF-RELATIVE-PATH and X-FF-PARENT-PATH headers"]
    Just preflightInode -> do
      (responseBody, responseStatusCode, responseStatusMessage) <- liftIO $ _preflightInode authToken preflightInode
      if responseStatusCode /= 200
        then sendErrorOrRedirect (Status responseStatusCode responseStatusMessage) responseBody
        else return ()

lookupPreflightInode :: MonadHandler m => m (Maybe PreflightInode)
lookupPreflightInode = do
  relativePath <- lookupHeader $ Data.CaseInsensitive.mk "X-FF-RELATIVE-PATH"
  parentPath <- lookupHeader $ Data.CaseInsensitive.mk "X-FF-PARENT-PATH"
  return $ PreflightInode <$> (Path . S8.unpack <$> parentPath) <*> (ClassyPrelude.singleton . Path . S8.unpack <$> relativePath)

lookupUploadedInode :: MonadHandler m => FileInfo -> m (Maybe UploadedInode)
lookupUploadedInode fileInfo = do
  let mimeType = Just (Text.unpack $ fileContentType fileInfo)
  relativePath <- lookupHeader $ Data.CaseInsensitive.mk "X-FF-RELATIVE-PATH"
  parentPath <- lookupHeader $ Data.CaseInsensitive.mk "X-FF-PARENT-PATH"
  size <- getRealFileSize fileInfo
  return $ UploadedInode <$> (Path . S8.unpack <$> parentPath) <*> (Path . S8.unpack <$> relativePath) <*> Just size <*> mimeType

lookupSingleFile :: [(Text.Text, FileInfo)] -> Maybe FileInfo
lookupSingleFile [("file", file)] = Just file
lookupSingleFile _ = Nothing

getRealFileSize :: MonadHandler m => FileInfo -> m Integer
getRealFileSize fileInfo = do
  liftIO $
    runConduitRes $
      fileSource fileInfo
        .| lengthCE

-- this creates the encryptionKey by generating it
makeAllocateResource :: KeyEncryptionKey -> Inode -> IO (IO (AES256, IV AES256), EncKey)
makeAllocateResource kek inode = do
  secretKey :: Crypto.Types.Key AES256 ByteString <- genSecretKey (undefined :: AES256) 32
  let Key keyBytes = secretKey
  ivBytes <- genRandomIV (undefined :: AES256)
  let encKey' = EncKey (encryptWithKek kek keyBytes) ivBytes
  return (return (initCipher secretKey, initIV ivBytes), encKey')

-- this takes the encryption information and encrypts and moves the file after the response has been send
makeFreeResource :: FileInfo -> Inode -> (AES256, IV AES256) -> IO ()
makeFreeResource fileInfo inode (cipher, iv) = do
  fileDest <- storeFile inode
  runConduitRes $
    fileSource fileInfo
      .| encryptConduit cipher iv mempty
      .| fileDest
