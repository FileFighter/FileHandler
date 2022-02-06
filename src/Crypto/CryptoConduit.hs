{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- |

module Crypto.CryptoConduit where
import ClassyPrelude
    ( ($),
      Integral(div),
      Monad((>>=), return),
      Num((-), (+), (*)),
      Ord(max),
      Semigroup((<>)),
      Maybe(Just, Nothing),
      maybe,
      fromMaybe,
      IsSequence(drop, splitAt),
      MonadIO,
      ByteString,
      error,
      length,
      null,
      (.) )
import ClassyPrelude.Conduit
    ( ($),
      Integral(div),
      Monad((>>=), return),
      Num((-), (+), (*)),
      Ord(max),
      Semigroup((<>)),
      Maybe(Just, Nothing),
      ByteString,
      MonadIO,
      ConduitT,
      (.),
      fromMaybe,
      maybe,
      error,
      length,
      null,
      IsSequence(drop, splitAt),
      await,
      yield )
import Crypto.Cipher.Types
import Crypto.Data.Padding

encryptConduit :: (BlockCipher c, Monad m) => c -> IV c -> ByteString -> ConduitT ByteString ByteString  m ()
encryptConduit cipher iv partialBlock = await >>= \case
  Nothing ->  yield $ cbcEncrypt cipher iv $ pad (PKCS7 (blockSize cipher)) partialBlock
  Just moreBytes -> let
          fullBlocks           = (length moreBytes + length partialBlock) `div` blockSize cipher
          (thisTime, nextTime) = splitAt (fullBlocks * blockSize cipher) (partialBlock <> moreBytes)
    in do
      iv' <- if null thisTime then return iv else do
        let cipherText            = cbcEncrypt cipher iv thisTime
            lastBlockOfCipherText = drop (length cipherText - blockSize cipher) cipherText
        yield cipherText
        maybe (error "makeIV failed") return $ makeIV lastBlockOfCipherText
      encryptConduit cipher iv' nextTime

decryptConduit :: (BlockCipher c, MonadIO m) => c -> IV c -> ByteString -> ConduitT ByteString ByteString m ()
decryptConduit cipher iv partialBlock = await >>= \case
  Nothing -> if null partialBlock  then return () else yield $ removePadding $ cbcDecrypt cipher iv partialBlock
  Just moreBytes -> let
          fullBlocks           = (length moreBytes + length partialBlock) `div` blockSize cipher
          (thisTime, nextTime) = splitAt ( max 0 (fullBlocks-1) * blockSize cipher) (partialBlock <> moreBytes)
    in do
      iv' <- if null thisTime then return iv else do
        let plainText            =   cbcDecrypt cipher iv thisTime
            lastBlockOfCipherText = drop (length thisTime - blockSize cipher) thisTime
        yield plainText
        maybe (error "makeIV failed") return $ makeIV lastBlockOfCipherText
      decryptConduit cipher iv' nextTime
  where removePadding =  fromMaybe "hallo da " . unpad (PKCS7 (blockSize cipher))
