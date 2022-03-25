-- |

module FileStorage where
import ClassyPrelude
import Yesod
import Models.Inode
import ClassyPrelude.Yesod
import System.Directory
import Data.Time
import GHC.IO.FD (openFile)



storeFile :: MonadResource m => Inode -> IO (ConduitT ByteString o m ())
storeFile inode = do
  let id = show $ fileSystemId inode
  createDirectoryIfMissing True $  take 1 id
  return  $sinkFileCautious (getPathFromFileId id)


retrieveFile :: MonadResource m => Inode ->ConduitT i ByteString m ()
retrieveFile inode= do
  let id = show $ fileSystemId inode
  sourceFile (getPathFromFileId id)

getPathFromFileId :: String -> String
getPathFromFileId id=take 1 id ++  ("/" ++id)

getInodeModifcationTime :: Inode -> IO UTCTime
getInodeModifcationTime inode =  do
  let id = show $ fileSystemId inode
  getModificationTime (getPathFromFileId id)


filterFiles :: Inode -> Bool
filterFiles file = case mimeType file of
  Nothing -> False
  _ -> True
