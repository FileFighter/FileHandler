{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Redundant bracket" #-}

-- |
module KeyStorage where

import ClassyPrelude (Handler, Maybe (Just, Nothing), MonadIO (liftIO), ReaderT, maybe, throwIO)
import ClassyPrelude.Yesod (ErrorResponse (NotFound), MonadHandler, PersistStoreRead (get), YesodPersist (YesodPersistBackend, runDB), return, ($))
import Crypto.Cipher.AES (AES256)
import Crypto.Cipher.Types (IV)
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

getEncKeyOrInternalError ::
  (MonadHandler m, PersistRecordBackend EncKey MongoContext, PersistQueryRead MongoContext) =>
  Inode ->
  KeyEncryptionKey ->
  ReaderT MongoContext m (Inode, (AES256, IV AES256))
getEncKeyOrInternalError inode kek = do
  mres :: (Maybe (EncKey)) <- get $ EncKeyKey (fileSystemId inode)
  case mres of
    Nothing -> sendInternalError
    Just (encKey) -> do
      let key = initCipher $ Key (decryptWithKek kek $ encKeyCipherKey encKey)
      let iv = (initIV $ encKeyCipherIv encKey)
      return (inode, (key, iv))

storeEncKey ::
  (MonadIO m, PersistRecordBackend EncKey MongoContext) =>
  Inode ->
  EncKey ->
  ReaderT MongoContext m ()
storeEncKey inode encKey = do
  let dbKey = EncKeyKey (fileSystemId inode)
  insertKey dbKey encKey
  get dbKey
  return ()

deleteEncKey ::
  (MonadHandler m, PersistRecordBackend EncKey MongoContext, PersistQueryRead MongoContext) =>
  Inode ->
  ReaderT MongoContext m ()
deleteEncKey inode = do
  delete (EncKeyKey $ fileSystemId inode)
