module Lib
    ( someFunc, typeFieldRename
    ) where

someFunc :: IO ()
someFunc = putStrLn "someFunc"


typeFieldRename :: String -> String
typeFieldRename "filesystem_type" = "type"
typeFieldRename name = name