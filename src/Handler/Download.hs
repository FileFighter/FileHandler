{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Handler.Download where

import ClassyPrelude
  ( Bool (True),
    Either (Right),
    FilePath,
    IO,
    Int,
    IsString (fromString),
    Maybe (..),
    Monad (return),
    MonadIO (..),
    Show (show),
    String,
    UTCTime,
    Utf8 (decodeUtf8),
    defaultTimeLocale,
    fromMaybe,
    maybe,
    pack,
    parseTimeM,
    unpack,
    void,
    ($),
    (++),
    (<$>),
  )
import ClassyPrelude.Yesod
  ( ConduitM,
    MonadHandler,
    MonadResource,
    TypedContent,
    addHeader,
    awaitForever,
    getYesod,
    invalidArgs,
    lookupGetParam,
    respondSource,
    runConduitRes,
    sendChunkBS,
    sendFile,
    sinkFile,
    yield,
    (.|),
  )
import Codec.Archive.Zip.Conduit.Zip
  ( ZipData (ZipDataSource),
    ZipEntry (..),
    ZipInfo (ZipInfo, zipComment),
    ZipOptions (..),
    zipStream,
  )
import Data.Aeson ()
import qualified Data.ByteString.Char8 as S8
import qualified Data.ByteString.Lazy as L
import Data.CaseInsensitive ()
import Data.Maybe ()
import qualified Data.Text as DataText
import Data.Time (TimeZone, getCurrentTimeZone, utcToLocalTime)
import FileStorage (getInodeModifcationTime, retrieveFile)
import FileSystemServiceClient.FileSystemServiceClient
  ( FileSystemServiceClient
      ( FileSystemServiceClient,
        getInodeContent
      ),
  )
import Foundation (App (App, fileSystemServiceClient), Handler)
import Logger ()
import Models.Inode
    ( Inode(lastUpdated, mimeType, name, size,path) )
import Models.RestApiStatus ()
import Network.HTTP.Req ()
import qualified Network.HTTP.Types as HttpTypes
import Network.Wai ()
import System.Directory (doesDirectoryExist, removeFile)
import System.Environment ()
import System.IO ()
import System.IO.Temp (emptySystemTempFile)
import UnliftIO.Resource (allocate)
import Utils.FileUtils ()
import Utils.HandlerUtils (handleApiCall, lookupAuth)
import Yesod.Core ()

getDownloadR :: Handler ClassyPrelude.Yesod.TypedContent
getDownloadR = do
  App {fileSystemServiceClient = FileSystemServiceClient {getInodeContent = getInodeContent}} <- ClassyPrelude.Yesod.getYesod
  bearerToken <- lookupAuth

  inodeIds <- lookupRequiredInodeIds
  (responseBody, responseStatusCode, responseStatusMessage, maybeFilename) <- liftIO $ getInodeContent bearerToken inodeIds
  inodes <- handleApiCall responseBody responseStatusCode responseStatusMessage
  case inodes of
    [singleInode] -> do
      ClassyPrelude.Yesod.addHeader "Content-Disposition" $ pack ("attachment; filename=\"" ++ Models.Inode.name singleInode ++ "\"")
      ClassyPrelude.Yesod.respondSource (S8.pack $ fromMaybe "application/octet-stream" (Models.Inode.mimeType singleInode)) $
        retrieveFile singleInode ClassyPrelude.Yesod..| ClassyPrelude.Yesod.awaitForever ClassyPrelude.Yesod.sendChunkBS
    multipleInodes -> do
      let archiveName = fromMaybe "Files" maybeFilename
      ClassyPrelude.Yesod.addHeader "Content-Disposition" ("attachment; filename=\"" ++ decodeUtf8 archiveName ++ ".zip" ++ "\"")
      (_, tempFile) <- allocate (makeAllocateResource multipleInodes) freeResource
      ClassyPrelude.Yesod.sendFile "application/zip" tempFile

makeAllocateResource :: [Models.Inode.Inode] -> IO FilePath
makeAllocateResource inodes = do
  path <- emptySystemTempFile "FileFighterFileHandler.zip"
  createZip inodes path
  return path

freeResource :: FilePath -> IO ()
freeResource = removeFile

createZip :: [Models.Inode.Inode] -> FilePath -> IO ()
createZip inodes filename = do
  timeZone <- liftIO getCurrentTimeZone
  ClassyPrelude.Yesod.runConduitRes $
    generateZipEntries inodes timeZone
      ClassyPrelude.Yesod..| void (zipStream zipOptions)
      ClassyPrelude.Yesod..| ClassyPrelude.Yesod.sinkFile filename

generateZipEntries :: (MonadIO m, ClassyPrelude.Yesod.MonadResource m) => [Models.Inode.Inode] -> TimeZone -> ClassyPrelude.Yesod.ConduitM () (ZipEntry, ZipData m) m ()
generateZipEntries (currentInode : nextInodes) timeZone = do
  let nameInZip = fromMaybe (Models.Inode.name currentInode) $ Models.Inode.path currentInode
  let size' = Models.Inode.size currentInode
  timeStamp <- liftIO $ getTimestampForInode currentInode
  let entry =
        ZipEntry
          { zipEntryName = Right $ fromString nameInZip,
            zipEntryTime = utcToLocalTime timeZone timeStamp,
            zipEntrySize = Nothing, -- Just (fromIntegral size'),
            zipEntryExternalAttributes = Nothing
          }

  ClassyPrelude.Yesod.yield (entry, ZipDataSource $retrieveFile currentInode)
  generateZipEntries nextInodes timeZone
  return ()
generateZipEntries [] _ = return ()

zipOptions :: ZipOptions
zipOptions =
  ZipOptions
    { zipOpt64 = True,
      zipOptCompressLevel = 9,
      zipOptInfo =
        ZipInfo
          { zipComment = ""
          }
    }

getTimestampForInode :: Models.Inode.Inode -> IO UTCTime
getTimestampForInode inode = do
  let maybeTimeStamp = convertUnixTimeStamp (Models.Inode.lastUpdated inode)
  case maybeTimeStamp of
    Just timeStamp -> return timeStamp
    Nothing -> getInodeModifcationTime inode

convertUnixTimeStamp :: Int -> Maybe UTCTime
convertUnixTimeStamp ts = do
  let i = parseTimeM True defaultTimeLocale "%s" (show ts) :: Maybe UTCTime
  case i of
    Just timeWithoutTimezone -> do
      Just timeWithoutTimezone
    Nothing -> Nothing

lookupRequiredInodeIds :: ClassyPrelude.Yesod.MonadHandler m => m String
lookupRequiredInodeIds = do
  maybeIds <- ClassyPrelude.Yesod.lookupGetParam "ids"
  maybe (ClassyPrelude.Yesod.invalidArgs ["Missing ids query parameter."]) return $ unpack <$> maybeIds
