-- |
{-# LANGUAGE OverloadedStrings #-}

module Utils.ZipFile where
import ClassyPrelude
import qualified Models.Inode
import Codec.Archive.Zip.Conduit.Zip
import ClassyPrelude.Conduit
import Data.Time
import FileStorage (retrieveFile, getInodeModifcationTime)

createZip :: [Models.Inode.Inode] -> FilePath -> IO ()
createZip inodes filename = do
  timeZone <- liftIO getCurrentTimeZone
  runConduitRes $
    generateZipEntries inodes timeZone
      .| void (zipStream zipOptions)
      .| sinkFile filename

generateZipEntries :: (MonadIO m, MonadResource m) => [Models.Inode.Inode] -> TimeZone -> ConduitM () (ZipEntry, ZipData m) m ()
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

  yield (entry, ZipDataSource $retrieveFile currentInode)
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
