module FilterTest (test) where

import Data.Time (LocalTime (LocalTime), TimeOfDay (TimeOfDay), TimeZone (TimeZone), ZonedTime (ZonedTime), fromGregorian)
import Filter as F (changeTimeZone, parseISO8601WithTimeZone, timeOfDayFromString, timeZoneFromOffsetString)
import Test.Hspec (describe, hspec, it, shouldBe)
import Prelude (Bool (..), IO, Maybe (Just, Nothing), Show (show), putStrLn, ($))

test :: IO ()
test = hspec $ do
  describe "timeOfDayFromString" $ do
    it "should convert HH:mm string to TimeOfDay" $ do
      F.timeOfDayFromString "00:00" `shouldBe` Just (TimeOfDay 0 0 0)
      F.timeOfDayFromString "23:59" `shouldBe` Just (TimeOfDay 23 59 0)
      F.timeOfDayFromString "12:34" `shouldBe` Just (TimeOfDay 12 34 0)
      F.timeOfDayFromString "12:04" `shouldBe` Just (TimeOfDay 12 4 0)
      F.timeOfDayFromString "01:23" `shouldBe` Just (TimeOfDay 1 23 0)

    it "should return Nothing for invalid input" $ do
      F.timeOfDayFromString "12:34:56" `shouldBe` Nothing
      F.timeOfDayFromString "1234" `shouldBe` Nothing
      F.timeOfDayFromString "12:345" `shouldBe` Nothing
      F.timeOfDayFromString "12:a4" `shouldBe` Nothing
      F.timeOfDayFromString "12:4" `shouldBe` Nothing
      F.timeOfDayFromString "1:23" `shouldBe` Nothing

  describe "timeZoneFromOffsetString" $ do
    it "should convert +09:00 to TimeZone" $ do
      F.timeZoneFromOffsetString "+09:00" `shouldBe` Just (TimeZone 540 False "")
      F.timeZoneFromOffsetString "-09:00" `shouldBe` Just (TimeZone (-540) False "")
      F.timeZoneFromOffsetString "+00:00" `shouldBe` Just (TimeZone 0 False "")
      F.timeZoneFromOffsetString "-00:00" `shouldBe` Just (TimeZone 0 False "")
      F.timeZoneFromOffsetString "+01:30" `shouldBe` Just (TimeZone 90 False "")
      F.timeZoneFromOffsetString "-01:30" `shouldBe` Just (TimeZone (-90) False "")
      F.timeZoneFromOffsetString "+12:00" `shouldBe` Just (TimeZone 720 False "")
      F.timeZoneFromOffsetString "-12:00" `shouldBe` Just (TimeZone (-720) False "")
      F.timeZoneFromOffsetString "Z" `shouldBe` Just (TimeZone 0 False "UTC")

    it "should return Nothing for invalid input" $ do
      F.timeZoneFromOffsetString "+09:00:00" `shouldBe` Nothing
      F.timeZoneFromOffsetString "+0900" `shouldBe` Nothing
      F.timeZoneFromOffsetString "+09:0" `shouldBe` Nothing
      F.timeZoneFromOffsetString "+9:00" `shouldBe` Nothing
      F.timeZoneFromOffsetString "+9:0" `shouldBe` Nothing
      F.timeZoneFromOffsetString "+9:0" `shouldBe` Nothing
      F.timeZoneFromOffsetString "+12:a0" `shouldBe` Nothing

  -- describe "changeTimeZone" $ do
  --   it "should change TimeZone" $ do
  --     -- ZonedTime は Eq を持っていないので、LocalTime と TimeZone を個別に比較する
  --     actualLocalTime `shouldBe` LocalTime (fromGregorian 2020 12 31) (TimeOfDay 15 0 0)
  --     actualTimeZone `shouldBe` TimeZone 0 False ""


-- describe "parseISO8601WithTimeZone" $ do
--   it "should parse ISO8601 string with TimeZone" $ do
--     let actual = F.parseISO8601WithTimeZone (TimeZone 540 False "") "2021-01-01T00:00:00Z"
--     case actual of
--       Just (ZonedTime actualLocalTime actualTimeZone) -> do
--         -- Compare LocalTime
--         let expectedLocalTime = LocalTime (fromGregorian 2021 1 1) (TimeOfDay 0 0 0)
--         actualLocalTime `shouldBe` expectedLocalTime
--
--         -- Compare TimeZone
--         let expectedTimeZone = TimeZone 0 False ""
--         actualTimeZone `shouldBe` expectedTimeZone
--       Nothing -> True `shouldBe` False
