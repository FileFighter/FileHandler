{-# LANGUAGE OverloadedStrings #-}
{-# HLINT ignore "Redundant bracket" #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module MockBackend where

import ClassyPrelude
import ClassyPrelude.Yesod (Application, Value, object, (.=))
import Control.Concurrent (ThreadId, forkIO)
import Data.Aeson (encode)
import GHC.Conc (killThread)
import Network.HTTP.Types.Status
import Network.Wai (Request (pathInfo), responseLBS)
import Network.Wai.Handler.Warp (run)

type MockResponses = [MockResponse]

data MockResponse = MockResponse {pathToRequest :: Text, returnValue :: Value, status :: Status}

withStubbedApi :: MockResponses -> IO () -> IO ()
withStubbedApi mockResponses action =
  bracket
    (withMockBackend mockResponses)
    killThread
    (const action)

withMockBackend :: MockResponses -> IO ThreadId
withMockBackend mockResponses = forkIO $ run 8080 $ makeApp mockResponses

makeApp :: MockResponses -> Application
makeApp mockResponses req send = do
  let path = pathInfo req
  case find (isRequestedPath path) mockResponses of
    Just mockResponse -> sendMockResponse mockResponse req send
    Nothing -> sendNotFoundError req send

isRequestedPath :: [Text] -> MockResponse -> Bool
isRequestedPath requestedPath (MockResponse {pathToRequest = pathToRequest}) = pathToRequest == intercalate "/" requestedPath

sendNotFoundError :: Application
sendNotFoundError _ send = do
  let response =
        object
          [ "message" .= ("Endpoint not found" :: String)
          ]
  send $
    responseLBS
      status404
      [("Content-Type", "application/json; charset=utf-8")]
      (encode response)

sendMockResponse :: MockResponse -> Application
sendMockResponse (MockResponse {returnValue = value, status = status}) _ send = do
  send $
    responseLBS
      status
      [("Content-Type", "application/json; charset=utf-8")]
      (encode value)
