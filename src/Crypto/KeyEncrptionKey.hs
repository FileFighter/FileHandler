{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-deprecations #-}

-- |
module Crypto.KeyEncrptionKey where

import ClassyPrelude
import Crypto.Cipher.AES
import Crypto.Cipher.Types
import Crypto.Data.Padding
import Crypto.Error
import Crypto.Init (initCipher, initIV)
import Crypto.KDF.BCryptPBKDF (Parameters (Parameters), generate)
import Crypto.RandomGen (genRandomIV)
import Crypto.Types (Key (Key))
import Data.ByteArray
import FileStorage (getPathFromFileId)
import Models.Inode
import System.Directory (doesFileExist)

kekSalt :: ByteString
kekSalt = "FileFighterFileHandlerWithSomeSalt"

data KeyEncryptionKey = KeyEncryptionKey
  { blockCipher :: AES256,
    initialIV :: IV AES256
  }

-- This should use the database later
getOrCreateKekIV :: IO ByteString
getOrCreateKekIV = do
  exists <- doesFileExist "kek.iv"
  if exists
    then readFile "kek.iv"
    else do
      ivBytes <- genRandomIV (undefined :: AES256)
      writeFile "kek.iv" ivBytes
      return ivBytes

createKeyEncrptionKey :: String -> ByteString -> KeyEncryptionKey
createKeyEncrptionKey password ivBytes = do
  let mInitIV = makeIV ivBytes
  case mInitIV of
    Nothing -> error "Failed to generate initialization vector for encrpting the keys."
    Just initIV -> do
      let secretKey :: Key AES256 ByteString = Key $ generateKeyfromPassword (fromString password)
      KeyEncryptionKey
        { blockCipher = initCipher secretKey,
          initialIV = initIV
        }

generateKeyfromPassword :: (ByteArray output) => ByteString -> output
generateKeyfromPassword password = do
  let params = Parameters 4 32
  generate params password kekSalt

encryptWithKek :: KeyEncryptionKey -> ByteString -> ByteString
encryptWithKek r@KeyEncryptionKey {blockCipher = cipher, initialIV = iv} = do
  cbcEncrypt cipher iv . pad (PKCS7 (blockSize cipher))

decryptWithKek :: KeyEncryptionKey -> ByteString -> ByteString
decryptWithKek r@KeyEncryptionKey {blockCipher = cipher, initialIV = iv} message = do
  let decrypted = cbcDecrypt cipher iv message
  fromMaybe
    decrypted
    (unpad (PKCS7 (blockSize cipher)) decrypted)

getKeyForInode :: KeyEncryptionKey -> Inode -> IO (AES256, IV AES256)
getKeyForInode kek inode = do
  key <- decryptWithKek kek <$> readFile ("keys/" <> getPathFromFileId (show $ fileSystemId inode) ++ ".key")
  iv <- readFile ("keys/" <> getPathFromFileId (show $ fileSystemId inode) ++ ".iv")

  return (initCipher $ Key key, initIV iv)
