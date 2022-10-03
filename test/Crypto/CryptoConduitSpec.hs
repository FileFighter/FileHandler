{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-deprecations #-}

-- |
module Crypto.CryptoConduitSpec where

import ClassyPrelude.Conduit (foldC, runConduitRes, (.|))
import ClassyPrelude.Yesod (yield)
import Crypto.Cipher.AES (AES256)
import Crypto.Cipher.Types (IV)
import Crypto.CryptoConduit (decryptConduit, encryptConduit)
import Crypto.Init (initCipher, initIV)
import Crypto.RandomGen (genRandomIV, genSecretKey)
import Crypto.Types (Key (Key))
import TestImport
  ( ByteString,
    Monoid (mempty),
    Spec,
    describe,
    it,
    readFile,
    shouldBe,
    shouldNotBe,
    undefined,
    ($),
  )

spec :: Spec
spec = do
  describe "CryptoConduit" $ do
    it "Encrypts and decrypts the message with random iv and key" $ do
      secretKey :: Crypto.Types.Key AES256 ByteString <- genSecretKey (undefined :: AES256) 32
      ivBytes <- genRandomIV (undefined :: AES256)
      let key = initCipher secretKey
      let iv :: IV AES256 = initIV ivBytes
      let message = "hallo"
      result <- runConduitRes $ yield message .| encryptConduit key iv mempty .| decryptConduit key iv mempty .| foldC
      result `shouldBe` message
      encrypted <- runConduitRes $ yield message .| encryptConduit key iv mempty .| foldC
      encrypted `shouldNotBe` message

    it "Encrypted and decrypts the message with give iv and key" $ do
      keyBytes <- readFile "./test/resources/key"
      ivBytes <- readFile "./test/resources/iv"
      let key :: AES256 = initCipher (Key keyBytes)
      let iv :: IV AES256 = initIV ivBytes
      let message = "hallo"
      result <- runConduitRes $ yield message .| encryptConduit key iv mempty .| decryptConduit key iv mempty .| foldC
      result `shouldBe` message
      encrypted <- runConduitRes $ yield message .| encryptConduit key iv mempty .| foldC
      encrypted `shouldNotBe` message
      encrypted `shouldBe` "\162Pu\DC3\168\170\161 '\157\SYNQ:\149W\203"
