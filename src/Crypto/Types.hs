-- |
{-# LANGUAGE GADTs #-}

module Crypto.Types where
import Crypto.Cipher.Types
import Data.ByteArray

data Key c a where
  Key :: (BlockCipher c, ByteArray a) => a -> Key c a
