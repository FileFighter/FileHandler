-- |
{-# LANGUAGE OverloadedStrings #-}

module Utils.ResponeUtils where
import Yesod
import Network.HTTP.Types
import Models.RestApiStatus

sendInternalError :: MonadHandler m => m a
sendInternalError =sendResponseStatus (Status 500 "Internal Server Error.") $ toJSON $ RestApiStatus "Internal Server Error" "500"
