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
import DBModels (EncKey (EncKey, encKeyCipherIv, encKeyCipherKey), EntityField (EncKeyFsId), Key (EncKeyKey))
import Database.Persist (Entity, PersistRecordBackend, PersistStoreWrite (insertKey))
import Database.Persist.MongoDB (Entity (Entity), MongoContext, PersistQueryRead (selectFirst), docToEntityEither, (==.))
import Foundation (App)
import Models.Inode (Inode (Inode, fileSystemId))
import Utils.HandlerUtils (sendInternalError)
import Yesod.Core.Types (HandlerContents (HCError))

--storeKey :: (MonadHandler m, YesodPersist App, PersistStoreWrite (YesodPersistBackend App), PersistStoreWrite (YesodPersistBackend App), YesodPersist App) => m EncKey
--storeKey :: (MonadHandler m) => ReaderT MongoContext Handler App -> Handler a
--storeKey = do
--runDB $ get ""
--return ()

getEncKeyOrInternalError ::
  (MonadHandler m, PersistRecordBackend (Entity EncKey) MongoContext, PersistQueryRead MongoContext) =>
  Inode ->
  KeyEncryptionKey ->
  ReaderT MongoContext m ((AES256, IV AES256))
getEncKeyOrInternalError inode kek = do
  mres :: (Maybe (Entity EncKey)) <- selectFirst [EncKeyFsId ==. fileSystemId inode] []
  case mres of
    Nothing -> sendInternalError
    Just (Entity _ encKey) -> do
      let key = initCipher $ Key (decryptWithKek kek $ encKeyCipherKey encKey)
      let iv = (initIV $ encKeyCipherIv encKey)
      return (key, iv)

storeEncKey ::
  (MonadHandler m, PersistRecordBackend (Entity EncKey) MongoContext, PersistQueryRead MongoContext) =>
  Inode ->
  EncKey ->
  ReaderT MongoContext m ()
storeEncKey inode encKey = do
  insertKey (EncKeyKey (fileSystemId inode)) encKey
