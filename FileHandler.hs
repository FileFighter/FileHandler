#!/usr/bin/env stack
{- stack
    --resolver lts-17.4
    --install-ghc
    runghc
    --package shakespeare
    --package wai-app-static
    --package wai-extra
    --package warp
    --package req
 -}

-- The code above is used for Haskell Stack's script interpreter
-- feature. For more information, see:
-- https://docs.haskellstack.org/en/stable/GUIDE/#script-interpreter
--
-- Note how we explicitly list an LTS Haskell snapshot
-- (https://www.stackage.org/lts-6.11) to ensure reproducibility. We
-- then state which packages need to be present to run this code.

-- Enable the OverloadedStrings extension, a commonly used feature.
{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}

-- We use the QuasiQuotes to embed Hamlet HTML templates inside
-- our source file.
{-# LANGUAGE QuasiQuotes #-}

-- Import the various modules that we'll use in our code.
import qualified Data.ByteString.Char8          as S8
import qualified Data.ByteString.Lazy           as L
import           Data.Functor.Identity
import qualified Network.HTTP.Types             as HttpTypes
import           Network.Wai
import           Network.Wai.Application.Static
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



-- | Entrypoint to our application
main :: IO ()
main = do
       -- For ease of setup, we want to have a "sanity" command line
    -- argument. We'll see how this is used in the Dockerfile
    -- later. Desired behavior:
    --
    -- * If we have the argument "sanity", immediately exit
    -- * If we have no arguments, run the server
    -- * Otherwise, error out
    args <- getArgs
    case args of
        ["sanity"] -> putStrLn "Sanity check passed, ready to roll!"
        [] -> do
            putStrLn "Launching DataHandler."
            -- Run our application (defined below) on port 5000
            run 5002 app
        _ -> error $ "Unknown arguments: " ++ show args

-- | Our main application
app :: Application
app req send =
    -- Route the request based on the path requested
    case pathInfo req of
        -- "/": send the HTML homepage contents
        [] -> send $ responseBuilder
                HttpTypes.status200
                [("Content-Type", "text/html; charset=utf-8")]
                (renderHtmlBuilder homepage)

        -- "/browse/...": use the file server to allow directory
        -- listings and downloading files
        ("browse":rest) ->
            -- We create a modified request that strips off the
            -- "browse" component of the path, so that the file server
            -- does not need to look inside a /browse/ directory
            let req' = req { pathInfo = rest }
             in fileServer req' send

        -- "/upload": handle a file upload
        ["upload"] -> upload req send

        ["download"] -> download req send

        -- anything else: 404
        _ -> send $ responseLBS
            HttpTypes.status404
            [("Content-Type", "text/plain; charset=utf-8")]
            "Endpoint does not exist"

-- | Create an HTML page which links to the /browse URL, and allows
-- for a file upload
homepage :: Html
homepage = [shamlet|
$doctype 5
<html>
    <head>
        <title>File server
    <body>
        <h1>File server
        <p>
            <a href=/browse/>Browse available files

        <form method=POST action=/upload enctype=multipart/form-data>
            <p>Upload a new file
            <input type=file name=file>
            <input type=submit>
|]

-- | Use the standard file server settings to serve files from the
-- current directory
fileServer :: Application
fileServer = staticApp (defaultFileServerSettings ".")

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
            [("Content-Type", "text/plain; charset=utf-8")]
            "No file parameter found"
        -- Got it!
        Just file -> do
            let content = fileContent file

            -- Write it out
            (responseBody, responseStatusCode, responseStatusMessage) <- postApi headers file (L.length content)
            case responseStatusCode of
                200 -> do
                    let d = (eitherDecode $ L.fromStrict responseBody ) :: (Either String PostResponseFile)
                    case d of
                        Left err -> send $ responseLBS
                                    HttpTypes.status200
                                    [ ("Content-Type", "text/plain: charset=utf-8")]
                                    (L.fromStrict $ S8.pack err)
                        Right fileObject -> do 
                                let id = fileSystemId fileObject
                                createDirectoryIfMissing True [head id]
                                L.writeFile (head id :  ("/" ++id)) content
                                send $ responseLBS
                                    HttpTypes.status200
                                    [ ("Content-Type", "text/plain: charset=utf-8")]
                                    "uploaded"
                _ ->
                    send $ responseLBS
                        (HttpTypes.mkStatus responseStatusCode (responseStatusMessage))
                        [ ("Content-Type", "text/plain: charset=utf-8")]
                        (L.fromStrict responseBody)


postApi :: [HttpTypes.Header] -> Network.Wai.Parse.FileInfo c -> GHC.Int.Int64 -> IO (S8.ByteString , Int, S8.ByteString)
postApi allheaders file size= runReq (defaultHttpConfig {httpConfigCheckResponse = httpConfigDontCheckResponse}) $ do
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
      (http "ptsv2.com" /: "t/vmlnd-1614506338/post") -- safe by construction URL
      (ReqBodyJson payload) -- use built-in options or add your own
      bsResponse  -- specify how to interpret response
      (header "X-FF-ParentID" (getOneHeader allheaders "X-FF-ParentID" ) <> header "Authorization" (getOneHeader allheaders "Authorization"))
     -- mempty -- query params, headers, explicit port number, etc.
  return (responseBody r, responseStatusCode r, responseStatusMessage r)



download :: Application
download req send = do
    let headers = requestHeaders req
    (responseBody, responseStatusCode, responseStatusMessage) <- getApi headers
    case responseStatusCode of
                200 -> do
                    let d = (eitherDecode $ L.fromStrict responseBody ) :: (Either String [GetResponseFile])
                    case d of
                        Left err -> send $ responseLBS
                                    HttpTypes.status200
                                    [ ("Content-Type", "text/plain: charset=utf-8")]
                                    (L.fromStrict $ S8.pack err)
                        Right files -> 
                            case files of
                                [fileObject] -> do
                                    let fileID = fsid fileObject 
                                    send $ responseFile
                                        HttpTypes.status200
                                        [ ("Content-Type", "text/plain: charset=utf-8")]
                                        (head fileID :  ("/" ++fileID))
                                        (Nothing)
                                [] ->
                                    send $ responseLBS
                                        HttpTypes.status200
                                        [ ("Content-Type", "text/plain: charset=utf-8")]
                                        "nothing"
                _ ->
                    send $ responseLBS
                        (HttpTypes.mkStatus responseStatusCode (responseStatusMessage))
                        [ ("Content-Type", "text/plain: charset=utf-8")]
                        (L.fromStrict responseBody)





getApi :: [HttpTypes.Header] -> IO (S8.ByteString , Int, S8.ByteString)
getApi allheaders= runReq (defaultHttpConfig {httpConfigCheckResponse = httpConfigDontCheckResponse}) $ do
  r <-
    req
      GET -- method
      (http "ptsv2.com" /: "t/vmlnd-1614506338/post") -- safe by construction URL
      NoReqBody -- use built-in options or add your own
      bsResponse  -- specify how to interpret response
      (header "X-FF-FileIDs" (getOneHeader allheaders "X-FF-FileIDs" ) <> header "Authorization" (getOneHeader allheaders "Authorization"))
     -- mempty -- query params, headers, explicit port number, etc.
  return (responseBody r, responseStatusCode r, responseStatusMessage r)



debug :: [Param] -> IO()
debug what =
    putStrLn (S8.unpack (snd (Prelude.head what)))


getOneHeader :: [HttpTypes.Header] -> String -> S8.ByteString
getOneHeader headers headerName=
    snd (head (Prelude.filter (\n -> fst n == (Data.CaseInsensitive.mk(S8.pack headerName ):: CI S8.ByteString)) headers))



httpConfigDontCheckResponse _ _ _ = Nothing



data PostResponseFile =
  PostResponseFile { fileSystemId  :: !String
           } deriving (Show,Generic)

instance FromJSON PostResponseFile
instance ToJSON PostResponseFile


data GetResponseFile =
  GetResponseFile { fsid :: !String  
            , name :: !String
           } deriving (Show,Generic)

instance FromJSON GetResponseFile
instance ToJSON GetResponseFile