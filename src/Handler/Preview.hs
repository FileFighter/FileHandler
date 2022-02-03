-- |
{-# LANGUAGE OverloadedStrings #-}

module Handler.Preview where
import Foundation
import Yesod.Core


import qualified Data.ByteString.Char8 as S8
import qualified Data.ByteString.Lazy as L
import qualified Network.HTTP.Types as HttpTypes
import qualified Data.Text
import Data.Aeson
import Data.Maybe (fromMaybe)




import Models.Inode
import Models.RestApiStatus
import Utils.RequestUtils
import Logger
import Network.HTTP.Req
import Network.Wai
import Utils.FileUtils
import FileSystemServiceClient.FileSystemServiceClient
import ClassyPrelude.Yesod
import FileStorage (retrieveFile)
import Utils.ResponeUtils (sendInternalError)
import qualified ClassyPrelude.Conduit as CB

getPreviewR :: Int -> String -> Handler TypedContent
getPreviewR id _ = do
  App{fileSystemServiceClient = FileSystemServiceClient{getInodeInfo=getInodeInfo}} <- getYesod
  bearerToken <- lookupAuth

  (responseBody, responseStatusCode, responseStatusMessage) <- liftIO $ getInodeInfo bearerToken $ show id
  case responseStatusCode of
    200 -> do
      case fromJSON responseBody of
        Success inode  -> do
          respondSource "text/plain" $ do
            retrieveFile inode .| awaitForever sendChunkBS

        Error _ -> sendInternalError
    _ -> sendResponseStatus (Status responseStatusCode responseStatusMessage) responseBody







lookupAuth :: MonadHandler m => m Text
lookupAuth = do
  authToken <- lookupCookie "token"
  maybe notAuthenticated return authToken

-- preview :: Application
-- preview req send = do
--   let headers = requestHeaders req
--       id = pathInfo req !! 2
--       redirectOnError = True --todo: make this a query param or something
--   restUrl <- getRestUrl
--   (responseBody, responseStatusCode, responseStatusMessage) <- previewApi headers id restUrl
--   logStdOut $ S8.unpack responseStatusMessage
--   case (responseStatusCode, redirectOnError) of
--     (200, _) -> do
--       let decoded = (eitherDecode $ L.fromStrict responseBody) :: (Either String Inode)
--       case decoded of
--         Left err ->
--           send $
--             responseLBS
--               HttpTypes.status500
--               [("Content-Type", "application/json; charset=utf-8")]
--               (encode $ RestApiStatus err "Internal Server Error")
--         Right file ->
--           let fileID = fileSystemId file
--               fileMimeType = fromMaybe "application/octet-stream" (mimeType file)
--               path = getPathFromFileId $ show fileID
--            in send $
--                 responseFile
--                   HttpTypes.status200
--                   [("Content-Type", S8.pack fileMimeType)]
--                   path
--                   Nothing
--     (_, True) -> do
--       let decoded = (eitherDecode $ L.fromStrict responseBody) :: (Either String RestApiStatus)
--       case decoded of
--         Left err ->
--           send $
--             responseLBS
--               HttpTypes.status500
--               [("Content-Type", "application/json; charset=utf-8")]
--               (encode $ RestApiStatus err "Internal Server Error")
--         Right status ->
--           let location =
--                 "/error?dest=" <> HttpTypes.urlEncode True (rawPathInfo req)
--                   <> "&message="
--                   <> HttpTypes.urlEncode True (S8.pack $ message status)
--            in send $ responseLBS HttpTypes.status303 [("Location", location)] ""
--     (_, False) ->
--       send $
--         responseLBS
--           (HttpTypes.mkStatus responseStatusCode responseStatusMessage)
--           [("Content-Type", "application/json; charset=utf-8")]
--           (L.fromStrict responseBody)
