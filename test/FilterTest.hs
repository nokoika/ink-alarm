module FilterTest (test) where

import Data.Time (TimeOfDay (TimeOfDay), ZonedTime (..))
import Test.Hspec (describe, hspec, it, shouldBe)
import Prelude (IO, Maybe (Just, Nothing), ($), Bool (..))

test :: IO ()
test = hspec $ do
  describe "dummy" $ do
    it "dummy" $ do
      True `shouldBe` True
