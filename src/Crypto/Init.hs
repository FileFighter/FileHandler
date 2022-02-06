-- |

module Crypto.Init where
import ClassyPrelude
import Crypto.Cipher.Types
import Crypto.Types (Key(Key))
import Data.ByteArray
import Crypto.Error

initIV :: (BlockCipher c) => ByteString -> IV c
initIV ivBytes = do
  case makeIV ivBytes of
    Nothing -> error "Failed to generate initialization vector."
    Just iv -> iv

initCipher :: (BlockCipher c, ByteArray a) => Key c a -> c
initCipher (Key k) = case cipherInit k of
  CryptoFailed e -> error "Failed to initialize cipher"
  CryptoPassed a -> a
