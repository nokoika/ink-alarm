module Test.Hash (test) where

import qualified Hash (sha256Hash)
import Test.Hspec (describe, hspec, it, shouldBe)
import Prelude (IO, ($))

test :: IO ()
test = hspec $ do
  describe "sha256Hash" $ do
    it "should hash a string" $ do
      Hash.sha256Hash "hello world" `shouldBe` "b94d27b9934d3e08a52e52d7da7dabfac484efe37a5380ee9088f7ace2efcde9"
