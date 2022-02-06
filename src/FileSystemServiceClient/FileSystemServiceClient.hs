{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UndecidableInstances #-}

-- |
module FileSystemServiceClient.FileSystemServiceClient where

import qualified Control.Monad.IO.Class
import Control.Monad.Trans.Resource
import Data.Aeson
import Data.ByteString.Char8 (ByteString)
import Data.Text
import Data.Text.Encoding (encodeUtf8)
import GHC.Generics
import Network.HTTP.Req
    ( (/:),
      (=:),
      defaultHttpConfig,
      header,
      http,
      jsonResponse,
      req,
      responseBody,
      responseHeader,
      responseStatusCode,
      responseStatusMessage,
      runReq,
      DELETE(DELETE),
      GET(GET),
      HttpConfig(httpConfigCheckResponse),
      NoReqBody(NoReqBody),
      POST(POST),
      ReqBodyJson(ReqBodyJson) )
import qualified Network.HTTP.Req as Req
import Settings
import ClassyPrelude hiding (pack, encodeUtf8)

data FileSystemServiceClient = FileSystemServiceClient
  { deleteInode :: Text -> String -> IO (Value, Int, ByteString),
    createInode :: Text -> UploadedInode -> String -> IO (Value, Int, ByteString),
    getInodeInfo ::Text -> String -> IO (Value, Int, ByteString),
    getInodeContent ::  Text -> String -> IO (Value, Int, ByteString, Maybe ByteString)
  }

data UploadedInode = UploadedInode
  { name :: String,
    path :: String,
    mimeType :: String,
    size :: String
  }
  deriving (Show, Generic)

instance ToJSON UploadedInode

httpConfigDontCheckResponse :: p1 -> p2 -> p3 -> Maybe a
httpConfigDontCheckResponse _ _ _ = Nothing

makeFileSystemServiceClient :: FileSystemServiceSettings -> FileSystemServiceClient
makeFileSystemServiceClient fileSystemServiceSettings =
  FileSystemServiceClient
    { deleteInode = makeDeleteInode fileSystemServiceSettings,
      createInode = makeCreateInode fileSystemServiceSettings,
      getInodeInfo = makeGetInodeInfo fileSystemServiceSettings,
      getInodeContent  = makeGetInodeContent fileSystemServiceSettings
    }

makeDeleteInode :: FileSystemServiceSettings -> Text -> String -> IO (Value, Int, ByteString)
makeDeleteInode r@FileSystemServiceSettings {url = url, port = port} authorization fileId = runReq (defaultHttpConfig {httpConfigCheckResponse = httpConfigDontCheckResponse}) $ do
  r <-
    req
      DELETE
      (http (pack url) /: "v1" /: "filesystem" /: pack fileId /: "delete")
      NoReqBody
      jsonResponse
      (oAuth2Bearer' (encodeUtf8 authorization) <> Req.port port) -- parentID not in Headers
  return (responseBody r, responseStatusCode r, responseStatusMessage r)

oAuth2Bearer' token = header "Authorization" ("Bearer " <> token)

makeCreateInode :: FileSystemServiceSettings -> Text -> UploadedInode -> String -> IO (Value, Int, ByteString)
makeCreateInode r@FileSystemServiceSettings {url = url, port = port} authorization uploadedInode fileId = runReq (defaultHttpConfig {httpConfigCheckResponse = httpConfigDontCheckResponse}) $ do
  r <-
    req
      POST -- method
      --(http (DataText.pack restUrl) /: "t/os3vu-1615111052/post")
      (http (pack url) /:  "v1" /: "filesystem" /: pack fileId /: "upload")
      (ReqBodyJson uploadedInode) -- use built-in options or add your own
      jsonResponse
      (oAuth2Bearer' (encodeUtf8 authorization) <> Req.port port) -- parentID not in Headers
  return (responseBody r, responseStatusCode r, responseStatusMessage r)

makeGetInodeInfo :: FileSystemServiceSettings ->  Text -> String -> IO (Value, Int, ByteString)
makeGetInodeInfo r@FileSystemServiceSettings {url = url, port = port} authorization id = runReq (defaultHttpConfig {httpConfigCheckResponse = httpConfigDontCheckResponse}) $ do
  r <-
    req
      GET -- method
      (http (pack url) /:  "v1" /: "filesystem" /: pack id /: "info") -- safe by construction URL
      --(http (DataText.pack restUrl) /: "v1" /: "filesystem" /:  id /: "info" ) -- safe by construction URL
      NoReqBody -- use built-in options or add your own
      jsonResponse -- specify how to interpret response
      (oAuth2Bearer' (encodeUtf8 authorization) <> Req.port port)
      -- mempty -- query params, headers, explicit port number, etc.
  return (responseBody r, responseStatusCode r, responseStatusMessage r)

makeGetInodeContent :: FileSystemServiceSettings ->  Text -> String -> IO (Value, Int, ByteString, Maybe ByteString)
makeGetInodeContent r@FileSystemServiceSettings {url = url, port = port} authorization ids = runReq (defaultHttpConfig {httpConfigCheckResponse = httpConfigDontCheckResponse}) $ do
  r <-
    req
      GET -- method
      (http (pack url) /:  "v1" /: "filesystem" /: "download") -- safe by construction URL
      -- (http (DataText.pack restUrl) /:"v1" /: "filesystem" /: DataText.pack  (S8.unpack (getOneHeader allHeaders "X-FF-IDS" )) /: "info")
      NoReqBody -- use built-in options or add your own
      jsonResponse -- specify how to interpret response
      (oAuth2Bearer' (encodeUtf8 authorization)
       <> Req.port port
       <> header "X-FF-IDS" (fromString ids)
       <> header "Cookie"  ("token=" <> encodeUtf8 authorization)
       <> (=:) "ids" ids )
      -- mempty -- query params, headers, explicit port number, etc.
  return (responseBody r, responseStatusCode r, responseStatusMessage r, responseHeader r "X-FF-NAME")
