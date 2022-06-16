{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Handler.Home where

import ClassyPrelude hiding (Handler)
import ClassyPrelude.Yesod (YesodPersist (runDB), insertEntity)
import FileSystemServiceClient.FileSystemServiceClient (PreflightInode (PreflightInode))
import Foundation
import Yesod.Core

getHomeR :: Handler String
getHomeR = do
  return "root Endpoint of the FileHandler Api, you should not have got here."
