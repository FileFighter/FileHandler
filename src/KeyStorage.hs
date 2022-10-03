{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Redundant bracket" #-}

-- |
module KeyStorage where

import ClassyPrelude (Bool (True), ByteString, Handler, IO, Maybe (Just, Nothing), MonadIO (liftIO), Monoid (mempty), ReaderT, const, maybe, throwIO)
import ClassyPrelude.Yesod (ConduitT, ErrorResponse (NotFound), MonadHandler, PersistStoreRead (get), ResourceT, YesodPersist (YesodPersistBackend, runDB), return, takeWhileCE, ($))
import ConduitHelper (idC)
import Crypto.Cipher.AES (AES256)
import Crypto.Cipher.Types (IV)
import Crypto.CryptoConduit (decryptConduit)
import Crypto.Init (initCipher, initIV)
import Crypto.KeyEncrptionKey (KeyEncryptionKey, decryptWithKek)
import Crypto.Types (Key (Key))
import DBModels (EncKey (EncKey, encKeyCipherIv, encKeyCipherKey), Key (EncKeyKey))
import Database.Persist (Entity, PersistRecordBackend, PersistStoreWrite (insertKey))
import Database.Persist.MongoDB (Entity (Entity), MongoContext, PersistQueryRead (selectFirst), PersistStoreWrite (delete), docToEntityEither, (==.))
import Foundation (App)
import Models.Inode (Inode (Inode, fileSystemId))
import Utils.HandlerUtils (sendInternalError)
import Yesod.Core.Types (HandlerContents (HCError))

getDecryptionFunctionMaybeFromDB inode kek = do
  case kek of
    Just kek -> runDB $ getEncKeyOrInternalError inode kek
    Nothing -> return (inode, idC)

getEncKeyOrInternalError ::
  (MonadHandler m, PersistRecordBackend EncKey MongoContext, PersistQueryRead MongoContext) =>
  Inode ->
  KeyEncryptionKey ->
  ReaderT MongoContext m (Inode, (ConduitT ByteString ByteString m ()))
getEncKeyOrInternalError inode kek = do
  mres :: (Maybe (EncKey)) <- get $ EncKeyKey (fileSystemId inode)
  case mres of
    Nothing -> sendInternalError
    Just (encKey) -> do
      let key :: AES256 = initCipher $ Key (decryptWithKek kek $ encKeyCipherKey encKey)
      let iv = (initIV $ encKeyCipherIv encKey)
      return (inode, decryptConduit key iv mempty)

storeEncKey ::
  (MonadIO m, PersistRecordBackend EncKey MongoContext) =>
  Inode ->
  Maybe EncKey ->
  ReaderT MongoContext m ()
storeEncKey inode (Just encKey) = do
  let dbKey = EncKeyKey (fileSystemId inode)
  insertKey dbKey encKey
  get dbKey
  return ()
storeEncKey inode (Nothing) = return ()

deleteEncKey ::
  (MonadHandler m, PersistRecordBackend EncKey MongoContext, PersistQueryRead MongoContext) =>
  Inode ->
  ReaderT MongoContext m ()
deleteEncKey inode = do
  delete (EncKeyKey $ fileSystemId inode)
