-- |
{-# LANGUAGE OverloadedStrings #-}

module Handler.Upload where


import Network.Wai
import Network.Wai.Parse

import qualified Network.HTTP.Types as HttpTypes
import qualified Data.ByteString.Char8 as S8
import qualified Data.ByteString.Lazy as L
import qualified Data.Text as DataText
import Control.Monad.State
import Control.Monad.Trans.Resource
import Network.HTTP.Req
import Data.CaseInsensitive
import System.Directory

import Models.Inode
import Utils.RequestUtils
import Utils.FileUtils
import Data.Aeson
import Models.RestApiStatus
import Logger
import Foundation
import Yesod.Core hiding (fileContentType)

postUploadR :: Handler ()
postUploadR =
  sendWaiApplication upload

upload :: Application
upload req send = runResourceT $
  withInternalState $
    \internalState ->
      do
        (_params, files) <- parseRequestBody (tempFileBackEnd internalState) req
        let headers = requestHeaders req
        -- debug (_params)
        -- Look for the file parameter called "file"
        case lookup "file" files of
          -- Not found, so return a 400 response
          Nothing ->
            send $
              responseLBS
                HttpTypes.status400
                [("Content-Type", "application/json; charset=utf-8")]
                (encode $ RestApiStatus "No file parameter found" "Bad Request")
          -- Got it!
          Just file -> do
            let content = fileContent file
            restUrl <- getRestUrl
            (responseBody, responseStatusCode, responseStatusMessage) <- postApi headers file restUrl (DataText.unpack $ pathInfo req !! 2)
            case responseStatusCode of
              201 -> do
                let d = (eitherDecode $ L.fromStrict responseBody) :: (Either String [Inode])
                case d of
                  Left err ->
                    send $
                      responseLBS
                        HttpTypes.status500
                        [("Content-Type", "application/json; charset=utf-8")]
                        (encode $ RestApiStatus err "Internal Server Error")
                  Right filesAndFolders ->
                    case filter filterFiles filesAndFolders of
                      [] ->
                        send $
                          responseLBS
                            HttpTypes.status500
                            [("Content-Type", "application/json; charset=utf-8")]
                            (encode $ RestApiStatus "No file found in rest response." "Internal Server Error")
                      [file] -> do
                        let id = show $ fileSystemId file
                        createDirectoryIfMissing True [head id]
                        copyFile content (getPathFromFileId id)
                        logStdOut ("Uploaded " ++ (head id : ("/" ++ id)))
                        send $
                          responseLBS
                            HttpTypes.status200
                            [("Content-Type", "application/json; charset=utf-8")]
                            (L.fromStrict responseBody)
              _ ->
                send $
                  responseLBS
                    (HttpTypes.mkStatus responseStatusCode responseStatusMessage)
                    [("Content-Type", "application/json; charset=utf-8")]
                    (L.fromStrict responseBody)

postApi :: [HttpTypes.Header] -> Network.Wai.Parse.FileInfo c -> String -> String -> IO (S8.ByteString, Int, S8.ByteString)
postApi allHeaders file restUrl fileId = runReq (defaultHttpConfig {httpConfigCheckResponse = httpConfigDontCheckResponse}) $ do
  let payload =
        object
          [ "name" .= S8.unpack (getOneHeader allHeaders "X-FF-NAME"), -- name and path are taken from headers
            "path" .= S8.unpack (getOneHeader allHeaders "X-FF-PATH"), -- because they could have been change by the user in the frontend
            "mimeType" .= S8.unpack (fileContentType file),
            "size" .= S8.unpack (getOneHeader allHeaders "X-FF-SIZE")
          ]

  r <-
    req
      POST -- method
      --(http (DataText.pack restUrl) /: "t/os3vu-1615111052/post")
      (http (DataText.pack restUrl) /: "api" /: "v1" /: "filesystem" /: DataText.pack fileId /: "upload")
      (ReqBodyJson payload) -- use built-in options or add your own
      bsResponse -- specify how to interpret response
      (header "Authorization" (getOneHeader allHeaders "Authorization") <> port 8080)
  return (responseBody r, responseStatusCode r, responseStatusMessage r)

