-- |

{-# LANGUAGE OverloadedStrings #-}
module Handler.Delete where
import Foundation
import Yesod.Core


import qualified Data.ByteString.Char8 as S8
import qualified Data.ByteString.Lazy as L
import qualified Network.HTTP.Types as HttpTypes
import qualified Data.Text as DataText
import Data.Aeson
import Data.Maybe (fromMaybe)
import Models.Inode
import Network.HTTP.Req
import Utils.RequestUtils
import Network.Wai
import Utils.FileUtils
import Logger
import Models.RestApiStatus
import System.Directory



deleteDeleteR :: Int -> Handler ()
deleteDeleteR  _ =
  sendWaiApplication delete
delete :: Application
delete req send = do
  logStdOut "requesting delete"
  let headers = requestHeaders req
  restUrl <- getRestUrl
  (responseBody, responseStatusCode, responseStatusMessage) <- deleteApi headers restUrl (DataText.unpack $ pathInfo req !! 2)
  case responseStatusCode of
    200 -> do
      let d = (eitherDecode $ L.fromStrict responseBody) :: (Either String [Inode])
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

deleteFile :: Inode -> IO ()
deleteFile file = removeFile $ getPathFromFileId (show $ fileSystemId file)
