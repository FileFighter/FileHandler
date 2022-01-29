{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
module Handler.Home where

import Foundation
import Yesod.Core



getHomeR :: Handler String
getHomeR =
  return "hallo"
