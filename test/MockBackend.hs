{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Redundant bracket" #-}

module MockBackend where

import           ClassyPrelude
import           ClassyPrelude.Yesod       (Application,
                                            ToJSON (toJSON),
                                            Value,
                                            object,
                                            (.=))
import           Control.Concurrent        (ThreadId,
                                            forkIO)
import           Control.Monad.Writer      (MonadWriter (tell),
                                            Writer,
                                            runWriter)
import           Data.Aeson                (encode)
import           GHC.Conc                  (killThread)
import           Network.HTTP.Types.Status
import           Network.Wai               (Request (pathInfo),
                                            responseLBS,
                                            strictRequestBody)
import           Network.Wai.Handler.Warp  (run)

type MockResponses = [MockResponse]

data MockResponse
  = MockResponse
      { pathToRequest :: Text
      , expectedBody  :: Value
      , returnValue   :: Value
      , status        :: Status
      }

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
  case filter (isRequestedPath path) mockResponses of
    [] -> sendNotFoundError "requested path wrong" req send
    mockResponses -> sendCorrectMockResponse mockResponses req send

sendCorrectMockResponse :: MockResponses -> Application
sendCorrectMockResponse responses req send = do
  bodyText <- (decodeUtf8 . toStrict) <$> strictRequestBody req
  putStrLn bodyText
  case (runWriter $ getRequestedResponse responses (bodyText)) of
    (Just response, _) -> sendMockResponse response req send
    (Nothing, log) -> do
      sendNotFoundError ("Body wrong, the following value where copared " <> log) req send

isRequestedPath :: [Text] -> MockResponse -> Bool
isRequestedPath requestedPath (MockResponse {pathToRequest = pathToRequest}) = pathToRequest == intercalate "/" requestedPath

getRequestedResponse :: MockResponses -> Text -> Writer Text (Maybe MockResponse)
getRequestedResponse responses body = do
  tell $ "Comparing " <> body
  findM (findCorrectBody body) responses

findCorrectBody :: Text -> MockResponse -> Writer Text Bool
findCorrectBody actualValue mockResponse = do
  let current = toStrict $ decodeUtf8 (encode $ expectedBody mockResponse)
  tell $ "Comparing with " <> current
  return $ actualValue == current

sendNotFoundError :: Text -> Application
sendNotFoundError message _ send = do
  let response =
        object
          [ "message" .= (message)
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

-- Searching

-- | Like 'find', but where the test can be monadic.
--
-- > findM (Just . isUpper) "teST"             == Just (Just 'S')
-- > findM (Just . isUpper) "test"             == Just Nothing
-- > findM (Just . const True) ["x",undefined] == Just (Just "x")
findM :: Monad m => (a -> m Bool) -> [a] -> m (Maybe a)
findM p = foldr (\x -> ifM (p x) (pure $ Just x)) (pure Nothing)

-- | Like @if@, but where the test can be monadic.
ifM :: Monad m => m Bool -> m a -> m a -> m a
ifM b t f = do b <- b; if b then t else f
