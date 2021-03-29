{-# LANGUAGE OverloadedStrings, DeriveGeneric, DuplicateRecordFields #-}


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
import Network.HTTP.Req
import Data.CaseInsensitive
import qualified Data.Text as DataText
import GHC.Int
import GHC.Generics
import System.Directory
import Control.Monad.State
import System.IO
import Control.Monad.Trans.Resource


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

        -- anything else: 404
        missingEndpoint -> send $ responseLBS
            HttpTypes.status404
            [("Content-Type", "application/json; charset=utf-8")]
            (encode $ RestApiStatus ("FileHandler: This endpoint does not exist." ++ show missingEndpoint) "Not Found")




-- | Handle file uploads, storing the file in the current directory
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
            filesize <- withFile content ReadMode hFileSize

            -- Write it out
            (responseBody, responseStatusCode, responseStatusMessage) <- postApi headers file filesize restUrl (DataText.unpack $ pathInfo  req!!2)
            case responseStatusCode of
                200 -> do
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
                                renameFile content (head id :  ("/" ++id))
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


postApi :: [HttpTypes.Header] -> Network.Wai.Parse.FileInfo c -> Integer -> String -> String -> IO (S8.ByteString , Int, S8.ByteString)
postApi allheaders file size restUrl fileId= runReq (defaultHttpConfig {httpConfigCheckResponse = httpConfigDontCheckResponse}) $ do
  let payload =
        object
          [ "name" .= S8.unpack (fileName file),
            "path" .= S8.unpack (fileName file),
            "mimetype" .= S8.unpack (fileContentType file),
            "size" .= size 
          ]


  -- One function—full power and flexibility, automatic retrying on timeouts
  -- and such, automatic connection sharing.
  r <-
    req
      POST -- method
      --(http (DataText.pack restUrl) /: "t/os3vu-1615111052/post") -- TODO: parentID in url
      (http (DataText.pack restUrl) /: "v1" /:"filesystem" /: DataText.pack fileId /: "upload") -- TODO: parentID in url
      (ReqBodyJson payload) -- use built-in options or add your own
      bsResponse  -- specify how to interpret response
      (header "Authorization" (getOneHeader allheaders "Authorization") <> port 8080) -- parentID not in Headers
     -- mempty -- query params, headers, explicit port number, etc.
  liftIO $ logStdOut $ show $responseBody r
  return (responseBody r, responseStatusCode r, responseStatusMessage r)



download :: Application
download req send = do
    let headers = requestHeaders req
    restUrl <- getRestUrl
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
                                    let fileID = fileSystemId (fileObject::RestResponseFile)
                                    let path = head fileID :  ("/" ++fileID)
                                    filesize <- withFile path ReadMode hFileSize
                                    send $ responseFile
                                        HttpTypes.status200
                                        [("Content-Disposition","attachment; filename=\"example-file.mp4\"")] -- TODO: use the correct mimetype 
                                        path
                                        Nothing
                                [] ->
                                    send $ responseLBS
                                        HttpTypes.status501
                                        [ ("Content-Type", "application/json; charset=utf-8")]
                                        (encode $ RestApiStatus "Uploaded" "Not Implemented")
                _ ->
                    send $ responseLBS
                        (HttpTypes.mkStatus responseStatusCode responseStatusMessage)
                        [ ("Content-Type", "application/json; charset=utf-8")]
                        (L.fromStrict responseBody)





getApi :: [HttpTypes.Header] -> String -> IO (S8.ByteString , Int, S8.ByteString)
getApi allheaders restUrl= runReq (defaultHttpConfig {httpConfigCheckResponse = httpConfigDontCheckResponse}) $ do
  r <-
    req
      GET -- method
    --  (http (DataText.pack restUrl) /: "t/vmlnd-1614506338/post") -- safe by construction URL
      (http (DataText.pack restUrl) /: "health" /: "fgsdhjfgh") -- safe by construction URL
      NoReqBody -- use built-in options or add your own
      bsResponse  -- specify how to interpret response
      (header "X-FF-IDS" (getOneHeader allheaders "X-FF-IDS" ) <> header "Authorization" (getOneHeader allheaders "Authorization") <> port 80)
     -- mempty -- query params, headers, explicit port number, etc.
  liftIO $ logStdOut $ show $responseBody r
  return (responseBody r, responseStatusCode r, responseStatusMessage r)



--debug :: [Param] -> IO()
 --debug what =
   -- putStrLn (S8.unpack (snd (Prelude.head what)))


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


httpConfigDontCheckResponse :: p1 -> p2 -> p3 -> Maybe a
httpConfigDontCheckResponse _ _ _ = Nothing


data RestResponseFile =
  GetResponseFile { 
            fileSystemId :: !String  
            , name :: !String
            , path :: !String
            , size :: Int 
            , createdByUser :: User
            , lastUpdated :: Int 
            , mimetype :: String
            , shared :: Bool 
           } deriving (Show,Generic)

instance FromJSON RestResponseFile
instance ToJSON RestResponseFile
data User = 
    User {
        userId :: Int 
        , username :: String
        , groups :: [String]
    } deriving (Show,Generic)

instance FromJSON User
instance ToJSON User


data RestApiStatus = 
    RestApiStatus {
        message :: !String
        , status :: !String
    } deriving (Show,Generic)

instance FromJSON RestApiStatus
instance ToJSON RestApiStatus

devCorsPolicy = Just CorsResourcePolicy {
        corsOrigins = Nothing
        , corsMethods = ["GET","POST"]
        , corsRequestHeaders = ["Authorization", "content-type","X-FF-IDS","X-FF-ID"]
        , corsExposedHeaders =  Just ["Content-Disposition"]
        , corsMaxAge = Just $ 60*60*24 -- one day
        , corsVaryOrigin = False
        , corsRequireOrigin = False 
        , corsIgnoreFailures = False
      }

-- maybe needed for prod?
prodCorsPolicy = Just CorsResourcePolicy {
        corsOrigins = Nothing
        , corsMethods = ["GET","POST"]
        , corsRequestHeaders = ["Authorization", "content-type","X-FF-IDS","X-FF-ID"]
        , corsExposedHeaders = Nothing
        , corsMaxAge = Just $ 60*60*24 -- one day
        , corsVaryOrigin = False
        , corsRequireOrigin = False 
        , corsIgnoreFailures = False
      }
      

getRestUrl :: IO String
getRestUrl=head <$> getArgs
