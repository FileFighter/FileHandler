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
import Network.HTTP.Req hiding (port)
import qualified Network.HTTP.Req as Req
import Settings
import Utils.RequestUtils

data FileSystemServiceClient = FileSystemServiceClient
  { deleteInode :: Text -> String -> IO (Value, Int, ByteString),
    createInode :: Text -> UploadedInode -> String -> IO (Value, Int, ByteString),
    getInodeInfo ::Text -> String -> IO (Value, Int, ByteString),
    downloadInode :: ()
  }

data UploadedInode = UploadedInode
  { name :: String,
    path :: String,
    mimeType :: String,
    size :: String
  }
  deriving (Show, Generic)

instance ToJSON UploadedInode

makeFileSystemServiceClient :: FileSystemServiceSettings -> FileSystemServiceClient
makeFileSystemServiceClient fileSystemServiceSettings =
  FileSystemServiceClient
    { deleteInode = makeDeleteInode fileSystemServiceSettings,
      createInode = makeCreateInode fileSystemServiceSettings,
      getInodeInfo = makeGetInodeInfo fileSystemServiceSettings,
      downloadInode = ()
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
