module LibTest (test) where

import Test.Hspec (hspec, it, shouldBe)
import Prelude (Bool (True), IO, ($))

test :: IO ()
test = hspec $ do
  it "empty test" $ do
    True `shouldBe` True
