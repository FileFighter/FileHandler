{-# LANGUAGE OverloadedStrings #-}

module Handler.Home where

import ClassyPrelude hiding (Handler)
import ClassyPrelude.Yesod (PersistStoreWrite (insertKey), YesodPersist (runDB), insertEntity)
import DBModels (EncKey (EncKey))
import FileSystemServiceClient.FileSystemServiceClient (PreflightInode (PreflightInode))
import Foundation
import Yesod.Core

getHomeR :: Handler String
getHomeR = do
  let encKey' = EncKey "" ""
  --runDB $ insertKey (EncKeyKey' "das") encKey'
  return "root Endpoint of the FileHandler Api, you should not have got here."
