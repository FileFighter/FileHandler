import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)
import Lib

main :: IO ()
main = hspec $
  describe "getPathFromFileId" $ do
    it "returns the first element of a list" $
      getPathFromFileId "34535345" `shouldBe` "3/34535345"

    it "returns the first element of an *arbitrary* list" $
      property $ \x xs -> head (x:xs) == (x :: Int)

    it "throws an exception if used with an empty list" $
      evaluate (head []) `shouldThrow` anyException
    
  