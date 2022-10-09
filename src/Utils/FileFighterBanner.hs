{-# LANGUAGE OverloadedStrings #-}

-- |
module Utils.FileFighterBanner where

import ClassyPrelude

printBanner :: IO ()
printBanner = do
  echo "  _____   _   _          _____   _           _       _                 "
  echo " |  ___| (_) | |   ___  |  ___| (_)   __ _  | |__   | |_    ___   _ __ "
  echo " | |_    | | | |  / _ \\ | |_    | |  / _\\`| | '_ \\  | __|  / _ \\ | '__|"
  echo " |  _|   | | | | |  __/ |  _|   | | | (_| | | | | | | |_  |  __/ | |   "
  echo " |_|     |_| |_|  \\___| |_|     |_|  \\__, | |_| |_|  \\__|  \\___| |_|   "
  echo "                                     |___/                             "
  echo "                   Version $1 Last updated: $2"
  echo "              Developed by Gimleux, Valentin, Open-Schnick.            "
  echo "             Development Blog: https://blog.filefighter.de           "
  echo "       The code can be found at: https://www.github.com/filefighter    "
  echo ""
  echo "-------------------------< $3 >---------------------------"
  echo ""
  where
    echo = putStrLn
