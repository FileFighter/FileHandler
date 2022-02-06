{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
module Handler.Home where

import Foundation
import Yesod.Core
import ClassyPrelude hiding (Handler)



getHomeR :: Handler String
getHomeR =
  return "hallo"
