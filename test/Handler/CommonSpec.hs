module Handler.CommonSpec (spec) where

import TestImport

spec :: Spec
spec = withApp $ do
  describe "home endpoint" $ do
    it "gives a 200" $ do
      get HomeR
      statusIs 200
