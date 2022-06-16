{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Handler.Home where

import ClassyPrelude hiding (Handler)
import Foundation
import Yesod.Core

getHomeR :: Handler String
getHomeR =
  return "/ Endpoint of the FileHandler Api, you should not have got here."
