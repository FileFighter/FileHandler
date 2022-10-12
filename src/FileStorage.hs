{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

-- |
module FileStorage where

import ClassyPrelude
import ClassyPrelude.Yesod
import Data.Time
import GHC.IO.FD (openFile)
import Models.Inode
import System.Directory
import Yesod

storeFile :: MonadResource m => Inode -> IO (ConduitT ByteString o m ())
storeFile inode = do
  let id = fileSystemId inode
  createDirectoryIfMissing True $ take 1 id
  return $ sinkFileCautious (getPathFromFileId id)

retrieveFile :: MonadResource m => Inode -> ConduitT i ByteString m ()
retrieveFile inode = do
  let id = fileSystemId inode
  sourceFile (getPathFromFileId id)

deleteFile :: (MonadLogger m, MonadIO m) => Inode -> m ()
deleteFile inode = do
  let id = fileSystemId inode
  let path = getPathFromFileId id
  liftIO (doesFileExist path)
    >>= \case
      False -> $(logError) $ "Could not delete file with path " <> pack path <> " because it does not exist."
      True -> liftIO $ removeFile $ getPathFromFileId id

getPathFromFileId :: String -> String
getPathFromFileId id = take 1 id ++ ("/" ++ id)

getInodeModifcationTime :: Inode -> IO UTCTime
getInodeModifcationTime inode =
  let id = fileSystemId inode
   in getModificationTime (getPathFromFileId id)

filterFiles :: Inode -> Bool
filterFiles file = case mimeType file of
  Nothing -> False
  _ -> True
