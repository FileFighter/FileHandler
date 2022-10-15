{-# LANGUAGE OverloadedStrings #-}

module MockBackend where

import ClassyPrelude
import ClassyPrelude.Yesod (Application, Value)
import Data.Aeson (encode)
import Network.HTTP.Types.Status
import Network.Wai (Request (pathInfo), responseLBS)
import Network.Wai.Handler.Warp (run)

type MockResponses = [MockResponse]

type MockResponse = (Text, Value, Status)

withMockBackend :: MockResponses -> IO ()
withMockBackend mockResponses = run 8080 $ makeApp mockResponses

makeApp :: MockResponses -> Application
makeApp mockResponses req send = do
  let path = pathInfo req
  case find (isRequestedPath path) mockResponses of
    Just mockResponse -> sendMockResponse mockResponse req send
    Nothing -> sendNotFoundError req send

isRequestedPath :: [Text] -> MockResponse -> Bool
isRequestedPath requestedPath (pathToMock, _, _) = pathToMock == intercalate "/" requestedPath

sendNotFoundError :: Application
sendNotFoundError _ send = do
  send $
    responseLBS
      status404
      [("Content-Type", "application/json; charset=utf-8")]
      ""

sendMockResponse :: MockResponse -> Application
sendMockResponse (_, value, status) _ send = do
  send $
    responseLBS
      status
      [("Content-Type", "application/json; charset=utf-8")]
      (encode value)
