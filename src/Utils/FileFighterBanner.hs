{-# LANGUAGE OverloadedStrings #-}

-- |
module Utils.FileFighterBanner where

import ClassyPrelude
import ClassyPrelude.Yesod (logInfo)
import Data.Version (showVersion)
import Paths_FileHandlerYesod (version)
import System.Log.FastLogger (LogStr, ToLogStr (toLogStr))

printBanner :: (LogStr -> IO ()) -> IO ()
printBanner log = do
  echo "  _____   _   _          _____   _           _       _                 "
  echo " |  ___| (_) | |   ___  |  ___| (_)   __ _  | |__   | |_    ___   _ __ "
  echo " | |_    | | | |  / _ \\ | |_    | |  / _\\`| | '_ \\  | __|  / _ \\ | '__|"
  echo " |  _|   | | | | |  __/ |  _|   | | | (_| | | | | | | |_  |  __/ | |   "
  echo " |_|     |_| |_|  \\___| |_|     |_|  \\__, | |_| |_|  \\__|  \\___| |_|   "
  echo "                                     |___/                             "
  echo $ "                         Version " <> showVersion version
  echo "              Developed by Gimleux, Valentin, Open-Schnick.            "
  echo "             Development Blog: https://blog.filefighter.de           "
  echo "       The code can be found at: https://www.github.com/filefighter    "
  echo ""
  echo "-------------------------< FileHandlerService >---------------------------"
  echo ""
  hFlush stdout
  where
    echo :: String -> IO ()
    echo msg = log . toLogStr $ msg <> "\n"
