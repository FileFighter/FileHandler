module Lib
    ( typeFieldRename, getPathFromFileId
    ) where



typeFieldRename :: String -> String
typeFieldRename "filesystemType" = "type"
typeFieldRename "type" = "filesystemType"
typeFieldRename name = name



getPathFromFileId :: String -> String
getPathFromFileId id=head id :  ("/" ++id)
