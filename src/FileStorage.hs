-- |

module FileStorage where
import Yesod
import Data.ByteString
import Models.Inode
import ClassyPrelude.Yesod
import System.Directory
import Data.Time



storeFile :: MonadResource m => Inode -> IO (ConduitT ByteString o m ())
storeFile inode = do
  let id = show $ fileSystemId inode
  createDirectoryIfMissing True [Prelude.head id]
  return  $sinkFile  (getPathFromFileId id)


retrieveFile :: MonadResource m => Inode ->ConduitT i ByteString m ()
retrieveFile inode= do
  let id = show $ fileSystemId inode
  sourceFile (getPathFromFileId id)

getPathFromFileId :: String -> String
getPathFromFileId id=Prelude.head id :  ("/" Prelude.++id)

getInodeModifcationTime :: Inode -> IO UTCTime
getInodeModifcationTime inode =  do
  let id = show $ fileSystemId inode
  getModificationTime (getPathFromFileId id)
