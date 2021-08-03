{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

-- Import the various modules that we'll use in our code.

import Codec.Archive.Zip
import Control.Monad.IO.Class
import Control.Monad.State
import Control.Monad.Trans.Resource
import Data.Aeson
import qualified Data.ByteString.Char8 as S8
import qualified Data.ByteString.Lazy as L
import Data.CaseInsensitive
import Data.Functor.Identity
import Data.Maybe (fromMaybe)
import qualified Data.Text as DataText
import GHC.Generics
import GHC.IO.Encoding (setLocaleEncoding)
import GHC.Int
import Lib
import Network.HTTP.Req
import qualified Network.HTTP.Types as HttpTypes
import Network.Wai
import Network.Wai.Application.Static
import Network.Wai.Handler.Warp
import Network.Wai.Middleware.Cors
import Network.Wai.Parse
import System.Directory
import System.Environment
import System.FilePath
import System.IO
import System.IO.Temp

-- | Entrypoint to our application
main :: IO ()
main = do
  -- For ease of setup, we want to have a "sanity" command line
  -- argument.
  --
  --  If we have the argument "sanity", immediately exit
  setLocaleEncoding utf8
  args <- getArgs
  case args of
    ["sanity"] -> putStrLn "Sanity check passed, ready to roll!"
    [restUrl, "dev"] -> do
      logStdOut "Launching DataHandler with dev profile"
      -- Run our application (defined below) on port 5000 with cors enabled
      run 5000 $ cors (const devCorsPolicy) app
    [restUrl, "stage"] -> do
      logStdOut "Launching DataHandler with stage profile"
      -- Run our application (defined below) on port 5000 with cors enabled
      run 5000 $ cors (const devCorsPolicy) app
    [restUrl, "prod"] -> do
      logStdOut "Launching DataHandler with prod profile"
      -- Run our application (defined below) on port 5000
      run 5000 app
    _ -> error $ "Unknown arguments: " ++ show args

-- | Our main application
app :: Application
app req send =
  -- Route the request based on the path requested
  case pathInfo req of
    -- "/upload": handle a file upload
    ["data", "upload", id] -> upload req send
    ["data", "download"] -> download req send
    ["data", "delete", id] -> delete req send
    ["data", "preview", id] -> preview req send
    ["data", "preview", id, _] -> preview req send
    ["data", "health"] -> health req send
    -- anything else: 404
    missingEndpoint ->
      send $
        responseLBS
          HttpTypes.status404
          [("Content-Type", "application/json; charset=utf-8")]
          (encode $ RestApiStatus ("FileHandler: This endpoint does not exist." ++ show missingEndpoint) "Not Found")

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
                let d = (eitherDecode $ L.fromStrict responseBody) :: (Either String [RestResponseFile])
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
          let d = (eitherDecode $ L.fromStrict responseBody) :: (Either String [RestResponseFile])
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
      let decoded = (eitherDecode $ L.fromStrict responseBody) :: (Either String RestResponseFile)
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

delete :: Application
delete req send = do
  logStdOut "requesting delete"
  let headers = requestHeaders req
  restUrl <- getRestUrl
  (responseBody, responseStatusCode, responseStatusMessage) <- deleteApi headers restUrl (DataText.unpack $ pathInfo req !! 2)
  case responseStatusCode of
    200 -> do
      let d = (eitherDecode $ L.fromStrict responseBody) :: (Either String [RestResponseFile])
      case d of
        Left err ->
          send $
            responseLBS
              HttpTypes.status500
              [("Content-Type", "application/json; charset=utf-8")]
              (encode $ RestApiStatus err "Internal Server Error")
        Right fileObjects -> do
          mapM_ deleteFile (filter filterFiles fileObjects)
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

deleteApi :: [HttpTypes.Header] -> String -> String -> IO (S8.ByteString, Int, S8.ByteString)
deleteApi allHeaders restUrl fileId = runReq (defaultHttpConfig {httpConfigCheckResponse = httpConfigDontCheckResponse}) $ do
  r <-
    req
      DELETE
      (http (DataText.pack restUrl) /: "api" /: "v1" /: "filesystem" /: DataText.pack fileId /: "delete")
      NoReqBody
      bsResponse
      (header "Authorization" (getOneHeader allHeaders "Authorization") <> port 8080) -- parentID not in Headers
  return (responseBody r, responseStatusCode r, responseStatusMessage r)

health :: Application
health req send = do
  deploymentType <- getDeploymentType
  files <- concat <$> (mapM listDirectoryRelative =<< (filterM doesDirectoryExist =<< listDirectory "."))
  actualFilesSize <- sum <$> mapM getFileSize files

  let response =
        object
          [ "version" .= ("0.2.1" :: String),
            "deploymentType" .= deploymentType,
            "actualFilesSize" .= actualFilesSize,
            "fileCount" .= length files
          ]
  send $
    responseLBS
      HttpTypes.status200
      [("Content-Type", "application/json; charset=utf-8")]
      (encode response)

getOneHeader :: [HttpTypes.Header] -> String -> S8.ByteString
getOneHeader headers headerName =
  case Prelude.filter (\n -> fst n == (Data.CaseInsensitive.mk (S8.pack headerName) :: CI S8.ByteString)) headers of
    [header] -> snd header
    _ -> ""

getDownloadQuery :: HttpTypes.Query -> Maybe String
getDownloadQuery [(param, Just value)] = if param == "ids" then Just (S8.unpack value) else Nothing
getDownloadQuery _ = Nothing

-- needed because buffering is causing problems with docker
logStdOut :: String -> IO ()
logStdOut text = do
  putStrLn text
  hFlush stdout

deleteFile :: RestResponseFile -> IO ()
deleteFile file = removeFile $ getPathFromFileId (show $ fileSystemId file)

filterFiles :: RestResponseFile -> Bool
filterFiles file = case filesystemType file of
  "FOLDER" -> False
  _ -> True

httpConfigDontCheckResponse :: p1 -> p2 -> p3 -> Maybe a
httpConfigDontCheckResponse _ _ _ = Nothing

data RestApiStatus = RestApiStatus
  { message :: !String,
    status :: !String
  }
  deriving (Show, Generic)

instance FromJSON RestApiStatus

instance ToJSON RestApiStatus

devCorsPolicy =
  Just
    CorsResourcePolicy
      { corsOrigins = Nothing,
        corsMethods = ["GET", "POST", "DELETE"],
        corsRequestHeaders = ["Authorization", "content-type", "X-FF-IDS", "X-FF-ID", "X-FF-NAME", "X-FF-PATH", "X-FF-SIZE"],
        corsExposedHeaders = Just ["Content-Disposition"],
        corsMaxAge = Just $ 60 * 60 * 24, -- one day
        corsVaryOrigin = False,
        corsRequireOrigin = False,
        corsIgnoreFailures = False
      }

getRestUrl :: IO String
getRestUrl = head <$> getArgs

getDeploymentType :: IO String
getDeploymentType = head . tail <$> getArgs

data User = User
  { userId :: Int,
    username :: String,
    groups :: [String]
  }
  deriving (Show, Generic)

instance FromJSON User

instance ToJSON User

data RestResponseFile = RestResponseFile
  { fileSystemId :: !Int,
    name :: String,
    path :: Maybe String,
    size :: Int,
    owner :: User,
    lastUpdatedBy :: User,
    lastUpdated :: Int,
    mimeType :: Maybe String,
    filesystemType :: String,
    shared :: Bool
  }
  deriving (Show, Generic)

instance FromJSON RestResponseFile where
  parseJSON =
    genericParseJSON
      defaultOptions
        { fieldLabelModifier = typeFieldRename,
          omitNothingFields = True
        }

listDirectoryRelative :: FilePath -> IO [FilePath]
listDirectoryRelative x = Prelude.map (x </>) <$> listDirectory x