{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UndecidableInstances #-}

-- |
module FileSystemServiceClient.FileSystemServiceClient where

import ClassyPrelude hiding (encodeUtf8, intercalate, pack)
import qualified Control.Monad.IO.Class
import Control.Monad.Trans.Resource
import Data.Aeson
import qualified Data.ByteString as S8
import Data.ByteString.Char8 (ByteString)
import Data.Text
import Data.Text.Encoding (encodeUtf8)
import GHC.Generics
import Models.Path (Path, fromMultiPiece, toByteString)
import Network.HTTP.Req
  ( DELETE (DELETE),
    GET (GET),
    HttpConfig (httpConfigCheckResponse),
    NoReqBody (NoReqBody),
    POST (POST),
    ReqBodyJson (ReqBodyJson),
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
    (/:),
    (=:),
  )
import qualified Network.HTTP.Req as Req
import Settings

data FileSystemServiceClient = FileSystemServiceClient
  { deleteInode :: Text -> [Text] -> IO (Value, Int, ByteString),
    createInode :: Text -> UploadedInode -> IO (Value, Int, ByteString),
    preflightInode :: Text -> PreflightInode -> IO (Value, Int, ByteString),
    getInodeInfo :: Text -> String -> IO (Value, Int, ByteString),
    getInodeContent :: Text -> Path -> IO (Value, Int, ByteString)
  }

data UploadedInode = UploadedInode
  { parentPath :: Path,
    relativePath :: Path,
    size :: Integer,
    mimeType :: String
  }
  deriving (Show, Generic)

instance ToJSON UploadedInode

data PreflightInode = PreflightInode
  { parentPath :: Path,
    relativePaths :: [Path]
  }
  deriving (Show, Generic)

instance ToJSON PreflightInode

httpConfigDontCheckResponse :: p1 -> p2 -> p3 -> Maybe a
httpConfigDontCheckResponse _ _ _ = Nothing

makeFileSystemServiceClient :: FileSystemServiceSettings -> FileSystemServiceClient
makeFileSystemServiceClient fileSystemServiceSettings =
  FileSystemServiceClient
    { deleteInode = makeDeleteInode fileSystemServiceSettings,
      createInode = makeCreateInode fileSystemServiceSettings,
      preflightInode = makePreflightInode fileSystemServiceSettings,
      getInodeInfo = makeGetInodeInfo fileSystemServiceSettings,
      getInodeContent = makeGetInodeContent fileSystemServiceSettings
    }

makeDeleteInode :: FileSystemServiceSettings -> Text -> [Text] -> IO (Value, Int, ByteString)
makeDeleteInode r@FileSystemServiceSettings {url = url, port = port} authorization path = runReq (defaultHttpConfig {httpConfigCheckResponse = httpConfigDontCheckResponse}) $ do
  r <-
    req
      DELETE
      (http (pack url) /: "api" /: "filesystem" /: "delete")
      NoReqBody
      jsonResponse
      ( oAuth2Bearer' (encodeUtf8 authorization) <> Req.port port
          <> header "X-FF-PATH" (toByteString $ fromMultiPiece path)
      )
  return (responseBody r, responseStatusCode r, responseStatusMessage r)

oAuth2Bearer' token = header "Authorization" ("Bearer " <> token)

makeCreateInode :: FileSystemServiceSettings -> Text -> UploadedInode -> IO (Value, Int, ByteString)
makeCreateInode r@FileSystemServiceSettings {url = url, port = port} authorization uploadedInode = runReq (defaultHttpConfig {httpConfigCheckResponse = httpConfigDontCheckResponse}) $ do
  r <-
    req
      POST -- method
      --(http (DataText.pack restUrl) /: "t/os3vu-1615111052/post")
      (http (pack url) /: "api" /: "filesystem" /: "upload")
      (ReqBodyJson uploadedInode) -- use built-in options or add your own
      jsonResponse
      (oAuth2Bearer' (encodeUtf8 authorization) <> Req.port port) -- parentID not in Headers
  return (responseBody r, responseStatusCode r, responseStatusMessage r)

makePreflightInode :: FileSystemServiceSettings -> Text -> PreflightInode -> IO (Value, Int, ByteString)
makePreflightInode r@FileSystemServiceSettings {url = url, port = port} authorization preflightInode = runReq (defaultHttpConfig {httpConfigCheckResponse = httpConfigDontCheckResponse}) $ do
  r <-
    req
      POST -- method
      (http (pack url) /: "api" /: "filesystem" /: "preflight")
      (ReqBodyJson preflightInode)
      jsonResponse
      (oAuth2Bearer' (encodeUtf8 authorization) <> Req.port port) -- parentID not in Headers
  return (responseBody r, responseStatusCode r, responseStatusMessage r)

makeGetInodeInfo :: FileSystemServiceSettings -> Text -> String -> IO (Value, Int, ByteString)
makeGetInodeInfo r@FileSystemServiceSettings {url = url, port = port} authorization id = runReq (defaultHttpConfig {httpConfigCheckResponse = httpConfigDontCheckResponse}) $ do
  r <-
    req
      GET -- method
      (http (pack url) /: "v1" /: "filesystem" /: pack id /: "info") -- safe by construction URL
      --(http (DataText.pack restUrl) /: "v1" /: "filesystem" /:  id /: "info" ) -- safe by construction URL
      NoReqBody -- use built-in options or add your own
      jsonResponse -- specify how to interpret response
      (oAuth2Bearer' (encodeUtf8 authorization) <> Req.port port)
  -- mempty -- query params, headers, explicit port number, etc.
  return (responseBody r, responseStatusCode r, responseStatusMessage r)

makeGetInodeContent :: FileSystemServiceSettings -> Text -> Path -> IO (Value, Int, ByteString)
makeGetInodeContent r@FileSystemServiceSettings {url = url, port = port} authorization path = runReq (defaultHttpConfig {httpConfigCheckResponse = httpConfigDontCheckResponse}) $ do
  r <-
    req
      GET -- method
      (http (pack url) /: "api" /: "filesystem" /: "download") -- safe by construction URL
      -- (http (DataText.pack restUrl) /:"v1" /: "filesystem" /: DataText.pack  (S8.unpack (getOneHeader allHeaders "X-FF-IDS" )) /: "info")
      NoReqBody -- use built-in options or add your own
      jsonResponse -- specify how to interpret response
      ( oAuth2Bearer' (encodeUtf8 authorization)
          <> Req.port port
          <> header "X-FF-PATH" (toByteString path)
      )
  -- mempty -- query params, headers, explicit port number, etc.
  return (responseBody r, responseStatusCode r, responseStatusMessage r)
