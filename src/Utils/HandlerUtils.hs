{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- |
module Utils.HandlerUtils where

import ClassyPrelude
  ( Bool (..),
    ByteString,
    Int,
    Monad (return, (>>=)),
    MonadIO (liftIO),
    Ord ((<), (<=)),
    Text,
    Utf8 (decodeUtf8),
    elem,
    maybe,
    otherwise,
    pack,
    print,
    putStr,
    putStrLn,
    ($),
    (&&),
    (.),
    (<$>),
  )
import Data.Aeson
import Foundation
import Models.RestApiStatus
import Network.HTTP.Types
import Network.Wai (rawPathInfo)
import Yesod
  ( ContentType,
    MonadHandler (HandlerSite),
    RedirectUrl,
    YesodRequest (reqAccept, reqWaiRequest),
    getRequest,
    lookupCookie,
    lookupGetParam,
    notAuthenticated,
    redirect,
    sendResponseStatus,
  )

sendInternalError :: MonadHandler m => m a
sendInternalError = sendResponseStatus (Status 500 "Internal Server Error.") $ toJSON $ RestApiStatus "Internal Server Error" "500"

handleApiCall' :: (MonadHandler m, FromJSON a, RedirectUrl (HandlerSite m) (Route App, [(Text, Text)])) => (Value, Int, ByteString) -> m a
handleApiCall' (body, statusCode, statusMessage) = handleApiCall body statusCode statusMessage

handleApiCall :: (MonadHandler m, FromJSON a, RedirectUrl (HandlerSite m) (Route App, [(Text, Text)])) => Value -> Int -> ByteString -> m a
handleApiCall body statusCode statusMessage
  | 200 <= statusCode && statusCode < 299 =
    case fromJSON body of
      Success value ->
        return value
      Error e -> do
        liftIO $ print e
        sendInternalError
  | 400 <= statusCode && statusCode < 500 = do
    liftIO $ print "4XX domain error"
    sendErrorOrRedirect (Status statusCode statusMessage) body --sendResponseStatus (Status statusCode statusMessage) body
  | otherwise = do
    liftIO $ print body
    sendInternalError

sendErrorOrRedirect :: (MonadHandler m, RedirectUrl (HandlerSite m) (Route App, [(Text, Text)])) => Status -> Value -> m a
sendErrorOrRedirect status body =
  lookupContentType "text/html" >>= \case
    True -> do
      case fromJSON body of
        Success value -> do
          rawPathInfo <- decodeUtf8 . rawPathInfo . reqWaiRequest <$> getRequest
          redirect (ErrorR, [("dest" :: Text, rawPathInfo :: Text), ("message" :: Text, pack $ message value :: Text)])
        Error _ -> sendInternalError
    False -> sendResponseStatus status body

lookupAuth :: MonadHandler m => m Text
lookupAuth = do
  authToken <- lookupCookie "token"
  authTokenParam <- lookupGetParam "token"
  maybe (maybe notAuthenticated return authTokenParam) return authToken

lookupContentType :: MonadHandler m => ContentType -> m Bool
lookupContentType contentType =
  elem contentType . reqAccept <$> getRequest
