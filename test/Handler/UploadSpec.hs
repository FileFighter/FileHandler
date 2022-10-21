{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Handler.UploadSpec (spec) where

import ClassyPrelude.Yesod (hAuthorization, status200)
import Control.Concurrent (killThread)
import Data.Aeson
import MockBackend (MockResponse (..), withMockBackend)
import Models.Inode (Inode (..))
import Models.User (User (User, privileges))
import TestImport

apiPrefix :: Text
apiPrefix = "api/api"

preflightMockResponse :: MockResponse
preflightMockResponse = MockResponse {pathToRequest = apiPrefix <> "/filesystem/preflight", returnValue = "", status = status200}

mockUser :: User
mockUser = User 1 "username" "privileges"

mockInode :: Inode
mockInode =
  Inode
    { fileSystemId = "abcd",
      name = "somefile.txt",
      path = Just "/someFolder/somefile.txt",
      mimeType = Just "text",
      size = 100,
      lastUpdated = 100,
      lastUpdatedBy = mockUser
    }

uploadMockResponse :: MockResponse
uploadMockResponse = MockResponse {pathToRequest = apiPrefix <> "/filesystem/upload", returnValue = toJSON [mockInode], status = status200}

withStubbedApi :: IO () -> IO ()
withStubbedApi action =
  bracket
    (withMockBackend [preflightMockResponse, uploadMockResponse])
    killThread
    (const action)

spec :: Spec
spec = withApp $
  around_ withStubbedApi $ do
    describe "Upload endpoint something" $ do
      it "Accepts file upload" $ do
        request $ do
          addFile "file" "./test/resources/someFile.txt" "text/plain"
          setUrl UploadR
          setMethod "POST"
          addRequestHeader (hAuthorization, "Bearer token")
          addRequestHeader ("X-FF-RELATIVE-PATH", "somefile.txt")
          addRequestHeader ("X-FF-PARENT-PATH", "/someFolder")
        statusIs 200
        safedFile <- liftIO $ readFile "./a/abcd"
        let expected = "Hallo\n"
        assertEq "Filecontent is correct" safedFile expected
