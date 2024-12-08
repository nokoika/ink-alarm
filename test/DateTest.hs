module DateTest (test) where

import Data.Time (TimeOfDay (TimeOfDay), ZonedTime (..))
import Date as D (changeTimeZone, timeOfDayFromString, timeZoneFromOffsetString, isWithinTimeRange)
import Test.Hspec (describe, hspec, it, shouldBe)
import qualified TestUtil as T (createLocalTime, createTimeZone, createUTCTime)
import Prelude (IO, Maybe (Just, Nothing), ($), Bool (..))

test :: IO ()
test = hspec $ do
  describe "dummy" $ do
    it "dummy" $ do
      True `shouldBe` True
  describe "timeOfDayFromString" $ do
    it "should convert HH:mm string to TimeOfDay" $ do
      D.timeOfDayFromString "00:00" `shouldBe` Just (TimeOfDay 0 0 0)
      D.timeOfDayFromString "23:59" `shouldBe` Just (TimeOfDay 23 59 0)
      D.timeOfDayFromString "12:34" `shouldBe` Just (TimeOfDay 12 34 0)
      D.timeOfDayFromString "12:04" `shouldBe` Just (TimeOfDay 12 4 0)
      D.timeOfDayFromString "01:23" `shouldBe` Just (TimeOfDay 1 23 0)

    it "should return Nothing for invalid input" $ do
      D.timeOfDayFromString "12:34:56" `shouldBe` Nothing
      D.timeOfDayFromString "1234" `shouldBe` Nothing
      D.timeOfDayFromString "12:345" `shouldBe` Nothing
      D.timeOfDayFromString "12:a4" `shouldBe` Nothing
      D.timeOfDayFromString "12:4" `shouldBe` Nothing
      D.timeOfDayFromString "1:23" `shouldBe` Nothing
      D.timeOfDayFromString "24:00" `shouldBe` Nothing
      D.timeOfDayFromString "00:60" `shouldBe` Nothing

  describe "timeZoneFromOffsetString" $ do
    it "should convert +09:00 to TimeZone" $ do
      D.timeZoneFromOffsetString "+09:00" `shouldBe` Just (T.createTimeZone 9 "")
      D.timeZoneFromOffsetString "-09:00" `shouldBe` Just (T.createTimeZone (-9) "")
      D.timeZoneFromOffsetString "+00:00" `shouldBe` Just (T.createTimeZone 0 "")
      D.timeZoneFromOffsetString "-00:00" `shouldBe` Just (T.createTimeZone 0 "")
      D.timeZoneFromOffsetString "+01:30" `shouldBe` Just (T.createTimeZone 1.5 "")
      D.timeZoneFromOffsetString "-01:30" `shouldBe` Just (T.createTimeZone (-1.5) "")
      D.timeZoneFromOffsetString "+12:00" `shouldBe` Just (T.createTimeZone 12 "")
      D.timeZoneFromOffsetString "-12:00" `shouldBe` Just (T.createTimeZone (-12) "")
      D.timeZoneFromOffsetString "+14:00" `shouldBe` Nothing
      D.timeZoneFromOffsetString "Z" `shouldBe` Just (T.createTimeZone 0 "UTC")

    it "should return Nothing for invalid input" $ do
      D.timeZoneFromOffsetString "+09:00:00" `shouldBe` Nothing
      D.timeZoneFromOffsetString "+0900" `shouldBe` Nothing
      D.timeZoneFromOffsetString "+09:0" `shouldBe` Nothing
      D.timeZoneFromOffsetString "+9:00" `shouldBe` Nothing
      D.timeZoneFromOffsetString "+9:0" `shouldBe` Nothing
      D.timeZoneFromOffsetString "+9:0" `shouldBe` Nothing
      D.timeZoneFromOffsetString "+12:a0" `shouldBe` Nothing
      D.timeZoneFromOffsetString "+15:00" `shouldBe` Nothing
      D.timeZoneFromOffsetString "+10:99" `shouldBe` Nothing

  describe "changeTimeZone" $ do
    it "should change TimeZone 1" $ do
      let ZonedTime {zonedTimeZone, zonedTimeToLocalTime} = D.changeTimeZone (T.createUTCTime 2021 1 1 0 0) (T.createTimeZone 9 "")
      -- UTC を JST にしているので、0時→9時になる
      zonedTimeToLocalTime `shouldBe` T.createLocalTime 2021 1 1 9 0
      zonedTimeZone `shouldBe` T.createTimeZone 9 ""

    it "should change TimeZone 2" $ do
      let ZonedTime {zonedTimeZone, zonedTimeToLocalTime} = D.changeTimeZone (T.createUTCTime 2021 1 1 0 0) (T.createTimeZone (-9) "")
      zonedTimeToLocalTime `shouldBe` T.createLocalTime 2020 12 31 15 0
      zonedTimeZone `shouldBe` T.createTimeZone (-9) ""

    it "should change TimeZone 3" $ do
      let ZonedTime {zonedTimeZone, zonedTimeToLocalTime} = D.changeTimeZone (T.createUTCTime 2021 1 1 0 0) (T.createTimeZone 0 "")
      zonedTimeToLocalTime `shouldBe` T.createLocalTime 2021 1 1 0 0
      zonedTimeZone `shouldBe` T.createTimeZone 0 ""

    it "should change TimeZone 4" $ do
      let ZonedTime {zonedTimeZone, zonedTimeToLocalTime} = D.changeTimeZone (T.createUTCTime 2021 1 1 0 0) (T.createTimeZone 1.5 "")
      zonedTimeToLocalTime `shouldBe` T.createLocalTime 2021 1 1 1 30
      zonedTimeZone `shouldBe` T.createTimeZone 1.5 ""

  describe "isWithinTimeRange" $ do
    it "should match" $ do
      D.isWithinTimeRange (TimeOfDay 0 0 0) (TimeOfDay 1 0 0) (ZonedTime (T.createLocalTime 2021 1 1 0 0) (T.createTimeZone 0 "")) `shouldBe` True
      D.isWithinTimeRange (TimeOfDay 0 0 0) (TimeOfDay 1 0 0) (ZonedTime (T.createLocalTime 2021 1 1 0 30) (T.createTimeZone 0 "")) `shouldBe` True
      D.isWithinTimeRange (TimeOfDay 0 0 0) (TimeOfDay 1 0 0) (ZonedTime (T.createLocalTime 2021 1 1 0 59) (T.createTimeZone 0 "")) `shouldBe` True
      -- timezone は無視
      D.isWithinTimeRange (TimeOfDay 0 0 0) (TimeOfDay 1 0 0) (ZonedTime (T.createLocalTime 2021 1 1 0 0) (T.createTimeZone 9 "")) `shouldBe` True

    it "should not match" $ do
      D.isWithinTimeRange (TimeOfDay 0 0 0) (TimeOfDay 1 0 0) (ZonedTime (T.createLocalTime 2021 1 1 1 1) (T.createTimeZone 0 "")) `shouldBe` False
      -- Range が 0:00 - 1:00 であり、end は境界を含まないので 2021/1/1 1:10 は含まれない
      D.isWithinTimeRange (TimeOfDay 0 0 0) (TimeOfDay 1 0 0) (ZonedTime (T.createLocalTime 2021 1 1 1 0) (T.createTimeZone 0 "")) `shouldBe` False

