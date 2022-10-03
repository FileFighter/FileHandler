{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-deprecations #-}

-- |
module Crypto.RandomGen where

import ClassyPrelude
import Crypto.Cipher.Types
import Crypto.Random.Types
import Crypto.Types (Key (Key))
import Data.ByteArray

-- | Generates a string of bytes (key) of a specific length for a given block cipher
genSecretKey ::
  forall m c a.
  (MonadRandom m, BlockCipher c, ByteArray a) =>
  -- |
  c ->
  -- |
  Int ->
  m (Key c a)
genSecretKey _ = fmap Key . getRandomBytes

-- | Generate a random initialization vector for a given block cipher
genRandomIV :: forall m c. (MonadRandom m, BlockCipher c) => c -> m ByteString
genRandomIV _ = do
  bytes :: ByteString <- getRandomBytes $ blockSize (undefined :: c)
  return bytes
