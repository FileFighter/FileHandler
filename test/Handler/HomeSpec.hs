{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Handler.HomeSpec (spec) where

import TestImport

spec :: Spec
spec = withApp $ do
  describe "root endpoint" $ do
    it "accepts get request and denies post request" $ do
      get HomeR
      statusIs 200
      request $ do
        setMethod "POST"
        setUrl HomeR
      statusIs 405
