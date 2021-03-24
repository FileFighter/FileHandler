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
import           Text.Blaze.Html.Renderer.Utf8
import           Text.Hamlet
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
            putStrLn "Launching DataHandler with dev profile"
            -- Run our application (defined below) on port 5000
            run 5000 $ cors (const devCorsPolicy) app
        [restUrl,"prod"] -> do
            putStrLn "Launching DataHandler with prod profile"
            -- Run our application (defined below) on port 5000
            run 5000 app
        _ -> error $ "Unknown arguments: " ++ show args

-- | Our main application
app :: Application 
app req send =
    -- Route the request based on the path requested
    case pathInfo req of
       
        -- "/upload": handle a file upload
        ["upload"] -> upload req send

        ["download"] -> download req send

        -- anything else: 404
        _ -> send $ responseLBS
            HttpTypes.status404
            [("Content-Type", "application/json; charset=utf-8")]
            (encode $ RestApiStatus "This endpoint does not exist." "Not Found")




-- | Handle file uploads, storing the file in the current directory
upload :: Application 
upload req send = do
    -- Parse the request body. We'll ignore parameters and just look
    -- at the files
    (_params, files) <- parseRequestBody lbsBackEnd req
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

            -- Write it out
            (responseBody, responseStatusCode, responseStatusMessage) <- postApi headers file (L.length content) restUrl
            case responseStatusCode of
                200 -> do
                    let d = (eitherDecode $ L.fromStrict responseBody ) :: (Either String PostResponseFile)
                    case d of
                        Left err -> send $ responseLBS
                                    HttpTypes.status500
                                    [ ("Content-Type", "application/json; charset=utf-8")]
                                    (encode $ RestApiStatus err "Internal Server Error")
                        Right fileObject -> do 
                                let id = fileSystemId (fileObject ::PostResponseFile)
                                createDirectoryIfMissing True [head id]
                                L.writeFile (head id :  ("/" ++id)) content
                                send $ responseLBS
                                    HttpTypes.status200
                                    [ ("Content-Type", "application/json; charset=utf-8")]
                                    (encode $ RestApiStatus "Uploaded" "Success")
                _ ->
                    send $ responseLBS
                        (HttpTypes.mkStatus responseStatusCode responseStatusMessage)
                        [ ("Content-Type", "application/json; charset=utf-8")]
                        (L.fromStrict responseBody)


postApi :: [HttpTypes.Header] -> Network.Wai.Parse.FileInfo c -> GHC.Int.Int64 -> String -> IO (S8.ByteString , Int, S8.ByteString)
postApi allheaders file size restUrl= runReq (defaultHttpConfig {httpConfigCheckResponse = httpConfigDontCheckResponse}) $ do
  let payload =
        object
          [ "name" .= S8.unpack (fileName file),
            "fileContentType" .= S8.unpack (fileContentType file),
            "size" .= size
          ]

  -- One function—full power and flexibility, automatic retrying on timeouts
  -- and such, automatic connection sharing.
  r <-
    req
      POST -- method
      (http (DataText.pack restUrl) /: "t/os3vu-1615111052/post") -- safe by construction URL
      (ReqBodyJson payload) -- use built-in options or add your own
      bsResponse  -- specify how to interpret response
      (header "X-FF-ID" (getOneHeader allheaders "X-FF-ID" ) <> header "Authorization" (getOneHeader allheaders "Authorization"))
     -- mempty -- query params, headers, explicit port number, etc.
  return (responseBody r, responseStatusCode r, responseStatusMessage r)



download :: Application
download req send = do
    let headers = requestHeaders req
    restUrl <- getRestUrl
    (responseBody, responseStatusCode, responseStatusMessage) <- getApi headers restUrl
    case responseStatusCode of
                200 -> do
                    let d = (eitherDecode $ L.fromStrict responseBody ) :: (Either String [GetResponseFile])
                    case d of
                        Left err -> send $ responseLBS
                                    HttpTypes.status501
                                    [ ("Content-Type", "application/json; charset=utf-8")]
                                    (L.fromStrict $ S8.pack err)
                        Right files -> 
                            case files of
                                [fileObject] -> do
                                    let fileID = fileSystemId (fileObject::GetResponseFile)
                                    let path = head fileID :  ("/" ++fileID)
                                    filesize <- withFile path ReadMode hFileSize
                                    send $ responseFile
                                        HttpTypes.status200
                                        [("X-FF-SIZE", S8.pack $ show filesize)] -- TODO: use the correct mimetype
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
      (http (DataText.pack restUrl) /: "t/vmlnd-1614506338/post") -- safe by construction URL
      NoReqBody -- use built-in options or add your own
      bsResponse  -- specify how to interpret response
      (header "X-FF-IDS" (getOneHeader allheaders "X-FF-IDS" ) <> header "Authorization" (getOneHeader allheaders "Authorization"))
     -- mempty -- query params, headers, explicit port number, etc.
  return (responseBody r, responseStatusCode r, responseStatusMessage r)



debug :: [Param] -> IO()
debug what =
    putStrLn (S8.unpack (snd (Prelude.head what)))


getOneHeader :: [HttpTypes.Header] -> String -> S8.ByteString
getOneHeader headers headerName=
    snd (head (Prelude.filter (\n -> fst n == (Data.CaseInsensitive.mk(S8.pack headerName ):: CI S8.ByteString)) headers))





httpConfigDontCheckResponse :: p1 -> p2 -> p3 -> Maybe a
httpConfigDontCheckResponse _ _ _ = Nothing



data PostResponseFile =
  PostResponseFile { fileSystemId  :: !String
           } deriving (Show,Generic)

instance FromJSON PostResponseFile
instance ToJSON PostResponseFile


data GetResponseFile =
  GetResponseFile { fileSystemId :: !String  
            , name :: !String
           } deriving (Show,Generic)




instance FromJSON GetResponseFile
instance ToJSON GetResponseFile

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
        , corsExposedHeaders = Nothing
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
