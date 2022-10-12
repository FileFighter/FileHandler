{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- |
module KeyStorage where

import ClassyPrelude (Bool (True), ByteString, Handler, IO, Int, Maybe (Just, Nothing), MonadIO (liftIO), Monoid (mempty), ReaderT, Traversable (mapM), any, const, length, mapM_, maybe, throwIO)
import ClassyPrelude.Yesod (ConduitT, ErrorResponse (NotFound), Filter, MonadHandler, PersistQueryRead (count), PersistStoreRead (get), ResourceT, YesodPersist (YesodPersistBackend, runDB), return, selectList, takeWhileCE, ($))
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
import Yesod.Core.Types (HandlerContents (HCError), HandlerFor)

maybeCountKeys ::
  (YesodPersist site, YesodPersistBackend site ~ MongoContext, PersistRecordBackend EncKey MongoContext) =>
  Maybe KeyEncryptionKey ->
  Yesod.Core.Types.HandlerFor site Int
maybeCountKeys Nothing = return 0
maybeCountKeys (Just kek) =
  runDB countEncKeys

countEncKeys ::
  (MonadIO m, PersistRecordBackend EncKey MongoContext, PersistQueryRead MongoContext) =>
  ReaderT MongoContext m Int
countEncKeys = do
  let filter = [] :: [Filter EncKey]
  count filter

maybeDeleteKeys ::
  (YesodPersist site, YesodPersistBackend site ~ MongoContext, PersistRecordBackend EncKey MongoContext) =>
  Maybe KeyEncryptionKey ->
  [Inode] ->
  Yesod.Core.Types.HandlerFor site ()
maybeDeleteKeys Nothing inodes = return ()
maybeDeleteKeys _ inodes =
  runDB $ mapM_ deleteEncKey inodes

deleteEncKey ::
  (MonadHandler m, PersistRecordBackend EncKey MongoContext, PersistQueryRead MongoContext) =>
  Inode ->
  ReaderT MongoContext m ()
deleteEncKey inode =
  delete (EncKeyKey $ fileSystemId inode)

getDecryptionFunctionMaybeFromDB ::
  (YesodPersist site, YesodPersistBackend site ~ MongoContext) =>
  Inode ->
  Maybe KeyEncryptionKey ->
  Yesod.Core.Types.HandlerFor site (Inode, ConduitT ByteString ByteString (Yesod.Core.Types.HandlerFor site) ())
getDecryptionFunctionMaybeFromDB inode kek =
  case kek of
    Just kek -> runDB $ getEncKeyOrInternalError inode kek
    Nothing -> return (inode, idC)

getEncKeyOrInternalError ::
  (MonadHandler m, PersistRecordBackend EncKey MongoContext, PersistQueryRead MongoContext) =>
  Inode ->
  KeyEncryptionKey ->
  ReaderT MongoContext m (Inode, ConduitT ByteString ByteString m ())
getEncKeyOrInternalError inode kek = do
  mres :: (Maybe EncKey) <- get $ EncKeyKey (fileSystemId inode)
  case mres of
    Nothing -> sendInternalError
    Just encKey -> do
      let key :: AES256 = initCipher $ Key (decryptWithKek kek $ encKeyCipherKey encKey)
      let iv = initIV $ encKeyCipherIv encKey
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
storeEncKey inode Nothing = return ()
