{-# LANGUAGE OverloadedStrings #-}

-- |
module Utils.ZipFile where

import ClassyPrelude
import ClassyPrelude.Conduit
import Codec.Archive.Zip.Conduit.Zip
import Crypto.Cipher.AES
import Crypto.Cipher.Types
import Crypto.CryptoConduit (decryptConduit)
import Data.Time
import FileStorage (getInodeModifcationTime, retrieveFile)
import qualified Models.Inode

createZip :: [(Models.Inode.Inode, (AES256, IV AES256))] -> FilePath -> IO ()
createZip inodes filename = do
  timeZone <- liftIO getCurrentTimeZone
  runConduitRes $
    generateZipEntries inodes timeZone
      .| void (zipStream zipOptions)
      .| sinkFile filename

generateZipEntries :: (MonadIO m, MonadResource m) => [(Models.Inode.Inode, (AES256, IV AES256))] -> TimeZone -> ConduitM () (ZipEntry, ZipData m) m ()
generateZipEntries ((currentInode, (key, iv)) : nextInodes) timeZone = do
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

  yield (entry, ZipDataSource $retrieveFile currentInode .| decryptConduit key iv mempty)
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
  parseTimeM True defaultTimeLocale "%s" (show ts) :: Maybe UTCTime
