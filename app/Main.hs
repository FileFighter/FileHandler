{-# LANGUAGE OverloadedStrings, DeriveGeneric, DuplicateRecordFields, TemplateHaskell #-}

module Main where

import Lib

-- Import the various modules that we'll use in our code.
import qualified Data.ByteString.Char8          as S8
import qualified Data.ByteString.Lazy           as L
import           Data.Functor.Identity
import qualified Network.HTTP.Types             as HttpTypes
import           Network.Wai
import           Network.Wai.Application.Static
import Network.Wai.Middleware.Cors
import           Network.Wai.Handler.Warp
import           Network.Wai.Parse
import           System.Environment
import           System.FilePath
import Control.Monad.IO.Class
import Data.Aeson
import Data.Aeson.TH(deriveJSON, defaultOptions, Options(fieldLabelModifier))
import Network.HTTP.Req
import Data.CaseInsensitive
import qualified Data.Text as DataText
import GHC.Int
import GHC.Generics
import System.Directory
import Control.Monad.State
import System.IO
import System.IO.Temp
import Control.Monad.Trans.Resource
import Codec.Archive.Zip



data User = 
    User {
        userId :: Int 
        , username :: String
        , groups :: [String]
    } deriving (Show,Generic)

instance FromJSON User
instance ToJSON User

data RestResponseFile =
  RestResponseFile { 
            fileSystemId :: !String  
            , name :: !String
            , path :: !String
            , size :: Int 
            , createdByUser :: User
            , lastUpdated :: Int 
            , mimetype :: String
            , filesystemType :: String
            , shared :: Bool 
           } deriving (Show,Generic)



instance FromJSON RestResponseFile where
  parseJSON = genericParseJSON defaultOptions {
                fieldLabelModifier = typeFieldRename }


-- | Entrypoint to our application
main :: IO ()
main = do
       -- For ease of setup, we want to have a "sanity" command line
    -- argument. We'll see how this is used in the Dockerfile
    -- later. Desired behavior:
    --
    --  If we have the argument "sanity", immediately exit
    --  If we have no arguments, run the server
    --  Otherwise, error out
    args <- getArgs
    case args of
        ["sanity"] -> putStrLn "Sanity check passed, ready to roll!"
        [restUrl,"dev"] -> do
            logStdOut "Launching DataHandler with dev profile"
            -- Run our application (defined below) on port 5000 with cors enabled
            run 5000 $ cors (const devCorsPolicy) app
        [restUrl,"prod"] -> do
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
        ["data","upload",id] -> upload req send

        ["data","download"] -> download req send

        ["data","delete",id] -> delete req send

        ["data","health"] -> health req send 

        -- anything else: 404
        missingEndpoint -> send $ responseLBS
            HttpTypes.status404
            [("Content-Type", "application/json; charset=utf-8")]
            (encode $ RestApiStatus ("FileHandler: This endpoint does not exist." ++ show missingEndpoint) "Not Found")




upload :: Application 
upload req send =do
    tempFileState <- createInternalState
    (_params, files) <- parseRequestBody (tempFileBackEnd tempFileState)  req
    let headers = requestHeaders req
   -- debug (_params)
    -- Look for the file parameter called "file"
    case lookup "file" files of
        -- Not found, so return a 400 response
        Nothing -> send $ responseLBS
            HttpTypes.status400
            [("Content-Type", "application/json; charset=utf-8")]
            (encode $ RestApiStatus "No file parameter found" "Bad Request")
        -- Got it!
        Just file -> do
            let content = fileContent file
            restUrl <- getRestUrl
            (responseBody, responseStatusCode, responseStatusMessage) <- postApi headers file restUrl (DataText.unpack $ pathInfo  req!!2)
            case responseStatusCode of
                201 -> do
                    let d = (eitherDecode $ L.fromStrict responseBody ) :: (Either String RestResponseFile)
                    case d of
                        Left err -> do
                                    closeInternalState tempFileState
                                    send $ responseLBS
                                      HttpTypes.status500
                                      [ ("Content-Type", "application/json; charset=utf-8")]
                                      (encode $ RestApiStatus err "Internal Server Error")
                        Right fileObject -> do 
                                let id = fileSystemId (fileObject ::RestResponseFile)
                                createDirectoryIfMissing True [head id]
                                renameFile content (getPathFromFileId id)
                                logStdOut ("Uploaded " ++ (head id :  ("/" ++id)))
                                closeInternalState tempFileState
                                send $ responseLBS
                                    HttpTypes.status200
                                    [ ("Content-Type", "application/json; charset=utf-8")]
                                     (L.fromStrict responseBody)
                _ -> do 
                    closeInternalState tempFileState
                    send $ responseLBS
                        (HttpTypes.mkStatus responseStatusCode responseStatusMessage)
                        [ ("Content-Type", "application/json; charset=utf-8")]
                        (L.fromStrict responseBody)


postApi :: [HttpTypes.Header] -> Network.Wai.Parse.FileInfo c -> String -> String -> IO (S8.ByteString , Int, S8.ByteString)
postApi allHeaders file restUrl fileId= runReq (defaultHttpConfig {httpConfigCheckResponse = httpConfigDontCheckResponse}) $ do
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
      (http (DataText.pack restUrl) /: "v1" /:"filesystem" /: DataText.pack fileId /: "upload")
      (ReqBodyJson payload) -- use built-in options or add your own
      bsResponse  -- specify how to interpret response
      (header "Authorization" (getOneHeader allHeaders "Authorization") <> port 8080)
  liftIO $ logStdOut $ S8.unpack (fileContentType file)
  liftIO $ logStdOut $ S8.unpack (responseBody r)
  return (responseBody r, responseStatusCode r, responseStatusMessage r)



download :: Application
download req send = do
    let headers = requestHeaders req
    restUrl <- getRestUrl
    logStdOut "download"
    (responseBody, responseStatusCode, responseStatusMessage) <- getApi headers restUrl
    case responseStatusCode of
                200 -> do
                    let d = (eitherDecode $ L.fromStrict responseBody ) :: (Either String [RestResponseFile])
                    case d of
                        Left err -> send $ responseLBS
                                    HttpTypes.status501
                                    [ ("Content-Type", "application/json; charset=utf-8")]
                                    (L.fromStrict $ S8.pack err)
                        Right files -> 
                            case files of
                                [fileObject] -> do
                                    let fileID = fileSystemId fileObject
                                        path = getPathFromFileId fileID
                                        realName = name fileObject
                                        fileMimeType = S8.pack $ mimetype fileObject
                                    send $ responseFile
                                        HttpTypes.status200
                                        [("Content-Disposition", S8.pack ("attachment; filename=\"" ++ realName ++ "\""))
                                        , ("Content-Type",fileMimeType)
                                        ]
                                        path
                                        Nothing
                                xs -> 
                                    withSystemTempFile "FileFighterFileHandler.zip" $ 
                                        \tmpFileName handle-> 
                                        do  let nameOfTheFolder = "NameOfTheFolderToDownload.zip"
                                            let ss = mapM (\n -> do inZipPath <- mkEntrySelector (path n)
                                                                    loadEntry Store inZipPath (getPathFromFileId (fileSystemId n))) 
                                                        xs
                                            createArchive tmpFileName ss
                                            send $ responseFile
                                                HttpTypes.status200
                                                [("Content-Disposition", S8.pack ("attachment; filename=\"" ++ nameOfTheFolder ++ "\""))
                                                , ("Content-Type","application/zip")
                                                ]
                                                tmpFileName
                                                Nothing
                _ ->
                    send $ responseLBS
                        (HttpTypes.mkStatus responseStatusCode responseStatusMessage)
                        [ ("Content-Type", "application/json; charset=utf-8")]
                        (L.fromStrict responseBody)





getApi :: [HttpTypes.Header] -> String -> IO (S8.ByteString , Int, S8.ByteString)
getApi allHeaders restUrl= runReq (defaultHttpConfig {httpConfigCheckResponse = httpConfigDontCheckResponse}) $ do
  r <-
    req
      GET -- method
     (http "ptsv2.com" /: "t/vmlnd-1614506338/post") -- safe by construction URLs
      --(http (DataText.pack restUrl) /: "v1"  /: "filesystem" /: "download") -- safe by construction URL
     -- (http (DataText.pack restUrl) /:"v1" /: "filesystem" /: DataText.pack  (S8.unpack (getOneHeader allHeaders "X-FF-IDS" )) /: "info") 
      NoReqBody -- use built-in options or add your own
      bsResponse  -- specify how to interpret response
      (header "X-FF-IDS" (getOneHeader allHeaders "X-FF-IDS" ) <> header "Authorization" (getOneHeader allHeaders "Authorization"))
     -- mempty -- query params, headers, explicit port number, etc.
  liftIO $ logStdOut $ S8.unpack (responseBody r)
  return (responseBody r, responseStatusCode r, responseStatusMessage r)


delete :: Application 
delete req send = do
    logStdOut "requesting delete"
    let headers = requestHeaders req
    restUrl <- getRestUrl
    (responseBody, responseStatusCode, responseStatusMessage) <- deleteApi headers restUrl (DataText.unpack $ pathInfo  req!!2)
    case responseStatusCode of
        200 -> do
            let d = (eitherDecode $ L.fromStrict responseBody ) :: (Either String [RestResponseFile])
            case d of
                Left err -> 
                    send $ responseLBS
                                HttpTypes.status500
                                [ ("Content-Type", "application/json; charset=utf-8")]
                                (encode $ RestApiStatus err "Internal Server Error")
                Right fileObjects -> do
                    mapM_ deleteFile (filter filterFiles fileObjects)
                    send $ responseLBS
                                    HttpTypes.status200
                                    [ ("Content-Type", "application/json; charset=utf-8")]
                                     (L.fromStrict responseBody)
        _ -> send $ responseLBS
                    (HttpTypes.mkStatus responseStatusCode responseStatusMessage)
                    [ ("Content-Type", "application/json; charset=utf-8")]
                    (L.fromStrict responseBody)


deleteApi :: [HttpTypes.Header] -> String -> String ->  IO (S8.ByteString , Int, S8.ByteString)
deleteApi allHeaders restUrl fileId = runReq (defaultHttpConfig {httpConfigCheckResponse = httpConfigDontCheckResponse}) $ do
    r <-
        req
        DELETE 
        --(http "ptsv2.com" /: "t/vmlnd-1614506338/post")
        (http (DataText.pack restUrl) /: "v1" /:"filesystem" /: DataText.pack fileId /: "delete") -- TODO: parentID in url
        NoReqBody  
        bsResponse
        (header "Authorization" (getOneHeader allHeaders "Authorization") <> port 8080) -- parentID not in Headers
    liftIO $ logStdOut $ S8.unpack (responseBody r)
    return (responseBody r, responseStatusCode r, responseStatusMessage r)

health :: Application 
health req send = do
    deploymentType <- getDeploymentType
    let response =
            object 
                [ "version" .= ("1.0.0" :: String),
                  "deploymentType" .= deploymentType
                ] 
    send $ responseLBS
                                    HttpTypes.status200
                                    [ ("Content-Type", "application/json; charset=utf-8")]
                                    (encode response)




getOneHeader :: [HttpTypes.Header] -> String -> S8.ByteString
getOneHeader headers headerName=
    case Prelude.filter (\n -> fst n == (Data.CaseInsensitive.mk(S8.pack headerName ):: CI S8.ByteString)) headers of 
        [header] -> snd header
        _ -> ""

-- needed because buffering is causing problems with docker
logStdOut :: String -> IO ()
logStdOut text = do
        putStrLn text
        hFlush stdout 



deleteFile :: RestResponseFile -> IO ()
deleteFile file = removeFile $ getPathFromFileId (fileSystemId file)

filterFiles :: RestResponseFile -> Bool
filterFiles file = case filesystemType file of 
                    "FOLDER" -> False
                    _ -> True


httpConfigDontCheckResponse :: p1 -> p2 -> p3 -> Maybe a
httpConfigDontCheckResponse _ _ _ = Nothing




data RestApiStatus = 
    RestApiStatus {
        message :: !String
        , status :: !String
    } deriving (Show,Generic)

instance FromJSON RestApiStatus
instance ToJSON RestApiStatus

devCorsPolicy = Just CorsResourcePolicy {
        corsOrigins = Nothing
        , corsMethods = ["GET","POST","DELETE"]
        , corsRequestHeaders = ["Authorization", "content-type","X-FF-IDS","X-FF-ID","X-FF-NAME","X-FF-PATH","X-FF-SIZE"]
        , corsExposedHeaders =  Just ["Content-Disposition"]
        , corsMaxAge = Just $ 60*60*24 -- one day
        , corsVaryOrigin = False
        , corsRequireOrigin = False 
        , corsIgnoreFailures = False
      }



getRestUrl :: IO String
getRestUrl=head <$> getArgs


getDeploymentType :: IO String
getDeploymentType=head .  tail <$> getArgs
