{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE ViewPatterns         #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}
module Application where

import Foundation
import Yesod.Core

import Handler.Home
import Handler.Download
import Handler.Upload
import Handler.Delete
import Handler.Preview
import Handler.Health

mkYesodDispatch "App" resourcesApp
