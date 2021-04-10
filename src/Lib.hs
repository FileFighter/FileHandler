module Lib
    ( typeFieldRename, getPathFromFileId
    ) where



typeFieldRename :: String -> String
typeFieldRename "filesystem_type" = "type"
typeFieldRename name = name



getPathFromFileId :: String -> String
getPathFromFileId id=head id :  ("/" ++id)
