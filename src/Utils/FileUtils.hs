-- |

module Utils.FileUtils where
import Models.Inode

getPathFromFileId :: String -> String
getPathFromFileId id=head id :  ("/" ++id)

filterFiles :: Inode -> Bool
filterFiles file = case filesystemType file of
  "FOLDER" -> False
  _ -> True
