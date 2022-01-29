
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
module Handler.Download where

import Foundation
import Yesod.Core


import Network.Wai
import Codec.Archive.Zip

import qualified Network.HTTP.Types as HttpTypes
import qualified Data.ByteString.Char8 as S8
import qualified Data.ByteString.Lazy as L
import qualified Data.Text as DataText
import Data.Maybe
import Data.CaseInsensitive

import Network.HTTP.Req
import System.Environment
import System.IO.Temp




import Models.Inode
import Models.RestApiStatus

import Utils.RequestUtils
import Utils.FileUtils

import Logger
import Data.Aeson

getDownloadR :: Handler ()
getDownloadR =
  sendWaiApplication download

download :: Application
download req send = do
  let headers = requestHeaders req
      queryParam = getDownloadQuery $ queryString req
      redirectOnError = True --todo: make this a query param or something
  case queryParam of
    Nothing ->
      send $
        responseLBS
          HttpTypes.status501
          [("Content-Type", "application/json; charset=utf-8")]
          "No ids parameter supplied."
    Just param -> do
      restUrl <- getRestUrl
      logStdOut "download"
      (responseBody, responseStatusCode, responseStatusMessage, fileNameHeader) <- getApi headers param restUrl
      case (responseStatusCode, redirectOnError) of
        (200, _) -> do
          let d = (eitherDecode $ L.fromStrict responseBody) :: (Either String [Inode])
          case d of
            Left err ->
              send $
                responseLBS
                  HttpTypes.status501
                  [("Content-Type", "application/json; charset=utf-8")]
                  (L.fromStrict $ S8.pack err)
            Right files ->
              case files of
                [fileObject] -> do
                  let fileID = fileSystemId fileObject
                      path = getPathFromFileId $ show fileID
                      realName = name fileObject
                      fileMimeType = fromMaybe "application/octet-stream" (mimeType fileObject)
                  send $
                    responseFile
                      HttpTypes.status200
                      [ ("Content-Disposition", S8.pack ("attachment; filename=\"" ++ realName ++ "\"")),
                        ("Content-Type", S8.pack fileMimeType)
                      ]
                      path
                      Nothing
                files ->
                  withSystemTempFile "FileFighterFileHandler.zip" $
                    \tmpFileName handle ->
                      do
                        let nameOfTheFolder = fromMaybe "Files" fileNameHeader
                        let ss =
                              mapM
                                ( \file -> do
                                    inZipPath <- mkEntrySelector $ fromMaybe (name file) (path file) -- either take the filename or path
                                    loadEntry Deflate inZipPath (getPathFromFileId (show $ fileSystemId file))
                                )
                                files
                        createArchive tmpFileName ss
                        send $
                          responseFile
                            HttpTypes.status200
                            [ ("Content-Disposition", S8.pack ("attachment; filename=\"" ++ S8.unpack nameOfTheFolder ++ ".zip" ++ "\"")),
                              ("Content-Type", "application/zip")
                            ]
                            tmpFileName
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
                    "/error?dest="
                      <> HttpTypes.urlEncode True (rawPathInfo req)
                      <> HttpTypes.urlEncode True (rawQueryString req)
                      <> "&message="
                      <> HttpTypes.urlEncode True (S8.pack $ message status)
               in send $ responseLBS HttpTypes.status303 [("Location", location)] ""
        (_, False) ->
          send $
            responseLBS
              (HttpTypes.mkStatus responseStatusCode responseStatusMessage)
              [("Content-Type", "application/json; charset=utf-8")]
              (L.fromStrict responseBody)

getApi :: [HttpTypes.Header] -> String -> String -> IO (S8.ByteString, Int, S8.ByteString, Maybe S8.ByteString)
getApi allHeaders param restUrl = runReq (defaultHttpConfig {httpConfigCheckResponse = httpConfigDontCheckResponse}) $ do
  r <-
    req
      GET -- method
      (http (DataText.pack restUrl) /: "api" /: "v1" /: "filesystem" /: "download") -- safe by construction URL
      -- (http (DataText.pack restUrl) /:"v1" /: "filesystem" /: DataText.pack  (S8.unpack (getOneHeader allHeaders "X-FF-IDS" )) /: "info")
      NoReqBody -- use built-in options or add your own
      bsResponse -- specify how to interpret response
      (header "X-FF-IDS" (getOneHeader allHeaders "X-FF-IDS") <> header "Cookie" (getOneHeader allHeaders "Cookie") <> port 8080 <> (=:) "ids" param) --PORT !!
      -- mempty -- query params, headers, explicit port number, etc.
  liftIO $ logStdOut $ show (getOneHeader allHeaders "Cookie")
  return (responseBody r, responseStatusCode r, responseStatusMessage r, responseHeader r "X-FF-NAME")

getDownloadQuery :: HttpTypes.Query -> Maybe String
getDownloadQuery [(param, Just value)] = if param == "ids" then Just (S8.unpack value) else Nothing
getDownloadQuery _ = Nothing
