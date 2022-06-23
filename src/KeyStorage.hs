{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

-- |
module KeyStorage where

import ClassyPrelude.Yesod (MonadHandler, YesodPersist (YesodPersistBackend, runDB), return, ($))
import Database.Persist (PersistStoreWrite (insertKey))

storeKey :: (MonadHandler m, YesodPersist site0, PersistStoreWrite (YesodPersistBackend site0), PersistStoreWrite (YesodPersistBackend site0)) => m ()
storeKey = do
  return ()
