{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- |
module Crypto.CryptoConduit where

import ClassyPrelude
  ( ByteString,
    Integral (div),
    IsSequence (drop, splitAt),
    Maybe (Just, Nothing),
    Monad (return, (>>=)),
    MonadIO,
    Num ((*), (+), (-)),
    Ord (max),
    Semigroup ((<>)),
    error,
    fromMaybe,
    length,
    maybe,
    null,
    ($),
    (.),
  )
import ClassyPrelude.Conduit
  ( ByteString,
    ConduitT,
    Integral (div),
    IsSequence (drop, splitAt),
    Maybe (Just, Nothing),
    Monad (return, (>>=)),
    MonadIO,
    Num ((*), (+), (-)),
    Ord (max),
    Semigroup ((<>)),
    await,
    error,
    fromMaybe,
    length,
    maybe,
    null,
    yield,
    ($),
    (.),
  )
import Crypto.Cipher.Types
  ( BlockCipher (blockSize, cbcDecrypt, cbcEncrypt),
    IV,
    makeIV,
  )
import Crypto.Data.Padding

type EncFunc m = ConduitT ByteString ByteString m ()

type DecFunc m = ConduitT ByteString ByteString m ()

encryptConduit :: (BlockCipher c, MonadIO m) => c -> IV c -> ByteString -> EncFunc m
encryptConduit cipher iv partialBlock =
  await >>= \case
    Nothing -> yield $ cbcEncrypt cipher iv $ pad (PKCS7 (blockSize cipher)) partialBlock
    Just moreBytes ->
      let fullBlocks = (length moreBytes + length partialBlock) `div` blockSize cipher
          (thisTime, nextTime) = splitAt (fullBlocks * blockSize cipher) (partialBlock <> moreBytes)
       in do
            iv' <-
              if null thisTime
                then return iv
                else do
                  let cipherText = cbcEncrypt cipher iv thisTime
                      lastBlockOfCipherText = drop (length cipherText - blockSize cipher) cipherText
                  yield cipherText
                  maybe (error "makeIV failed") return $ makeIV lastBlockOfCipherText
            encryptConduit cipher iv' nextTime

decryptConduit :: (BlockCipher c, MonadIO m) => c -> IV c -> ByteString -> DecFunc m
decryptConduit cipher iv partialBlock =
  await >>= \case
    Nothing -> if null partialBlock then return () else yield $ removePadding $ cbcDecrypt cipher iv partialBlock
    Just moreBytes ->
      let fullBlocks = (length moreBytes + length partialBlock) `div` blockSize cipher
          (thisTime, nextTime) = splitAt (max 0 (fullBlocks -1) * blockSize cipher) (partialBlock <> moreBytes)
       in do
            iv' <-
              if null thisTime
                then return iv
                else do
                  let plainText = cbcDecrypt cipher iv thisTime
                      lastBlockOfCipherText = drop (length thisTime - blockSize cipher) thisTime
                  yield plainText
                  maybe (error "makeIV failed") return $ makeIV lastBlockOfCipherText
            decryptConduit cipher iv' nextTime
  where
    removePadding = fromMaybe "hallo da " . unpad (PKCS7 (blockSize cipher))
