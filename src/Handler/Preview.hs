-- |
{-# LANGUAGE OverloadedStrings #-}

module Handler.Preview where
import Foundation
import Yesod.Core


import qualified Data.ByteString.Char8 as S8
import qualified Data.ByteString.Lazy as L
import qualified Network.HTTP.Types as HttpTypes
import qualified Data.Text as DataText
import Data.Aeson
import Data.Maybe (fromMaybe)




import Models.Inode
import Models.RestApiStatus
import Utils.RequestUtils
import Logger
import Network.HTTP.Req
import Network.Wai
import Utils.FileUtils

getPreviewR :: Int -> Handler ()
getPreviewR _ =
  sendWaiApplication preview

preview :: Application
preview req send = do
  let headers = requestHeaders req
      id = pathInfo req !! 2
      redirectOnError = True --todo: make this a query param or something
  restUrl <- getRestUrl
  (responseBody, responseStatusCode, responseStatusMessage) <- previewApi headers id restUrl
  logStdOut $ S8.unpack responseStatusMessage
  case (responseStatusCode, redirectOnError) of
    (200, _) -> do
      let decoded = (eitherDecode $ L.fromStrict responseBody) :: (Either String Inode)
      case decoded of
        Left err ->
          send $
            responseLBS
              HttpTypes.status500
              [("Content-Type", "application/json; charset=utf-8")]
              (encode $ RestApiStatus err "Internal Server Error")
        Right file ->
          let fileID = fileSystemId file
              fileMimeType = fromMaybe "application/octet-stream" (mimeType file)
              path = getPathFromFileId $ show fileID
           in send $
                responseFile
                  HttpTypes.status200
                  [("Content-Type", S8.pack fileMimeType)]
                  path
                  Nothing
    (_, True) -> do
      let decoded = (eitherDecode $ L.fromStrict responseBody) :: (Either String RestApiStatus)
      case decoded of
        Left err ->
          send $
            responseLBS
              HttpTypes.status500
              [("Content-Type", "application/json; charset=utf-8")]
              (encode $ RestApiStatus err "Internal Server Error")
        Right status ->
          let location =
                "/error?dest=" <> HttpTypes.urlEncode True (rawPathInfo req)
                  <> "&message="
                  <> HttpTypes.urlEncode True (S8.pack $ message status)
           in send $ responseLBS HttpTypes.status303 [("Location", location)] ""
    (_, False) ->
      send $
        responseLBS
          (HttpTypes.mkStatus responseStatusCode responseStatusMessage)
          [("Content-Type", "application/json; charset=utf-8")]
          (L.fromStrict responseBody)

previewApi :: [HttpTypes.Header] -> DataText.Text -> String -> IO (S8.ByteString, Int, S8.ByteString)
previewApi allHeaders id restUrl = runReq (defaultHttpConfig {httpConfigCheckResponse = httpConfigDontCheckResponse}) $ do
  r <-
    req
      GET -- method
      (http (DataText.pack restUrl) /: "api" /: "v1" /: "filesystem" /: id /: "info") -- safe by construction URL
      --(http (DataText.pack restUrl) /: "v1" /: "filesystem" /:  id /: "info" ) -- safe by construction URL
      NoReqBody -- use built-in options or add your own
      bsResponse -- specify how to interpret response
      (header "Cookie" (getOneHeader allHeaders "Cookie") <> port 8080) --PORT !!
      -- mempty -- query params, headers, explicit port number, etc.
  liftIO $ logStdOut "Requested fileinfo"
  return (responseBody r, responseStatusCode r, responseStatusMessage r)
