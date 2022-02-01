{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE ViewPatterns      #-}
module Foundation where

import Yesod.Core
import Settings (AppSettings)
import FileSystemServiceClient.FileSystemServiceClient

data App = App
  { appSettings :: AppSettings
  , fileSystemServiceClient :: FileSystemServiceClient
  }

mkYesodData "App" $(parseRoutesFile "routes.yesodroutes")

instance Yesod App where
