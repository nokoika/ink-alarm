module Test.Date (test) where

import qualified Data.Time as T
import qualified Date as D (changeTimeZone, hasTimeRangesIntersect, intersectTimeRangesWithLocalTime, isWithinTimeOfDay, timeOfDayFromString, timeRangesIntersect, timeZoneFromOffsetString)
import Test.Hspec (describe, hspec, it, shouldBe)
import qualified TestUtil as TU
import Prelude (Bool (..), IO, Maybe (Just, Nothing), ($))

test :: IO ()
test = hspec $ do
  describe "timeOfDayFromString" $ do
    it "should convert HH:mm string to TimeOfDay" $ do
      D.timeOfDayFromString "00:00" `shouldBe` Just (T.TimeOfDay 0 0 0)
      D.timeOfDayFromString "23:59" `shouldBe` Just (T.TimeOfDay 23 59 0)
      D.timeOfDayFromString "12:34" `shouldBe` Just (T.TimeOfDay 12 34 0)
      D.timeOfDayFromString "12:04" `shouldBe` Just (T.TimeOfDay 12 4 0)
      D.timeOfDayFromString "01:23" `shouldBe` Just (T.TimeOfDay 1 23 0)

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
      D.timeZoneFromOffsetString "+09:00" `shouldBe` Just (TU.createTimeZone 9 "")
      D.timeZoneFromOffsetString "-09:00" `shouldBe` Just (TU.createTimeZone (-9) "")
      D.timeZoneFromOffsetString "+00:00" `shouldBe` Just (TU.createTimeZone 0 "")
      D.timeZoneFromOffsetString "-00:00" `shouldBe` Just (TU.createTimeZone 0 "")
      D.timeZoneFromOffsetString "+01:30" `shouldBe` Just (TU.createTimeZone 1.5 "")
      D.timeZoneFromOffsetString "-01:30" `shouldBe` Just (TU.createTimeZone (-1.5) "")
      D.timeZoneFromOffsetString "+12:00" `shouldBe` Just (TU.createTimeZone 12 "")
      D.timeZoneFromOffsetString "-12:00" `shouldBe` Just (TU.createTimeZone (-12) "")
      D.timeZoneFromOffsetString "+14:00" `shouldBe` Just (TU.createTimeZone 14 "")
      D.timeZoneFromOffsetString "Z" `shouldBe` Just (TU.createTimeZone 0 "UTC")

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
      let T.ZonedTime {zonedTimeZone, zonedTimeToLocalTime} = D.changeTimeZone (TU.createUTCTime 2021 1 1 0 0) (TU.createTimeZone 9 "")
      -- UTC を JST にしているので、0時→9時になる
      zonedTimeToLocalTime `shouldBe` TU.createLocalTime 2021 1 1 9 0
      zonedTimeZone `shouldBe` TU.createTimeZone 9 ""

    it "should change TimeZone 2" $ do
      let T.ZonedTime {zonedTimeZone, zonedTimeToLocalTime} = D.changeTimeZone (TU.createUTCTime 2021 1 1 0 0) (TU.createTimeZone (-9) "")
      zonedTimeToLocalTime `shouldBe` TU.createLocalTime 2020 12 31 15 0
      zonedTimeZone `shouldBe` TU.createTimeZone (-9) ""

    it "should change TimeZone 3" $ do
      let T.ZonedTime {zonedTimeZone, zonedTimeToLocalTime} = D.changeTimeZone (TU.createUTCTime 2021 1 1 0 0) (TU.createTimeZone 0 "")
      zonedTimeToLocalTime `shouldBe` TU.createLocalTime 2021 1 1 0 0
      zonedTimeZone `shouldBe` TU.createTimeZone 0 ""

    it "should change TimeZone 4" $ do
      let T.ZonedTime {zonedTimeZone, zonedTimeToLocalTime} = D.changeTimeZone (TU.createUTCTime 2021 1 1 0 0) (TU.createTimeZone 1.5 "")
      zonedTimeToLocalTime `shouldBe` TU.createLocalTime 2021 1 1 1 30
      zonedTimeZone `shouldBe` TU.createTimeZone 1.5 ""

  describe "isWithinTimeOfDay" $ do
    it "should match" $ do
      D.isWithinTimeOfDay (T.TimeOfDay 0 0 0, T.TimeOfDay 1 0 0) (T.TimeOfDay 0 0 0) `shouldBe` True
      D.isWithinTimeOfDay (T.TimeOfDay 0 0 0, T.TimeOfDay 1 0 0) (T.TimeOfDay 0 30 0) `shouldBe` True
      D.isWithinTimeOfDay (T.TimeOfDay 0 0 0, T.TimeOfDay 1 0 0) (T.TimeOfDay 0 59 0) `shouldBe` True
      -- 日をまたぐ
      D.isWithinTimeOfDay (T.TimeOfDay 23 0 0, T.TimeOfDay 1 0 0) (T.TimeOfDay 23 0 0) `shouldBe` True
      D.isWithinTimeOfDay (T.TimeOfDay 23 0 0, T.TimeOfDay 1 0 0) (T.TimeOfDay 23 59 0) `shouldBe` True
      D.isWithinTimeOfDay (T.TimeOfDay 23 0 0, T.TimeOfDay 1 0 0) (T.TimeOfDay 0 0 0) `shouldBe` True
      D.isWithinTimeOfDay (T.TimeOfDay 23 0 0, T.TimeOfDay 1 0 0) (T.TimeOfDay 0 59 0) `shouldBe` True

    it "should not match" $ do
      D.isWithinTimeOfDay (T.TimeOfDay 0 0 0, T.TimeOfDay 1 0 0) (T.TimeOfDay 1 0 0) `shouldBe` False
      D.isWithinTimeOfDay (T.TimeOfDay 0 0 0, T.TimeOfDay 1 0 0) (T.TimeOfDay 1 1 0) `shouldBe` False
      -- 日をまたぐ
      D.isWithinTimeOfDay (T.TimeOfDay 23 0 0, T.TimeOfDay 1 0 0) (T.TimeOfDay 1 0 0) `shouldBe` False
      D.isWithinTimeOfDay (T.TimeOfDay 23 0 0, T.TimeOfDay 1 0 0) (T.TimeOfDay 1 1 0) `shouldBe` False
      D.isWithinTimeOfDay (T.TimeOfDay 23 0 0, T.TimeOfDay 1 0 0) (T.TimeOfDay 6 0 0) `shouldBe` False
      -- 25時は想定しない
      D.isWithinTimeOfDay (T.TimeOfDay 23 0 0, T.TimeOfDay 25 0 0) (T.TimeOfDay 0 0 0) `shouldBe` False

  let d1 = T.TimeOfDay 3 0 0
  let d2 = T.TimeOfDay 5 0 0
  let d3 = T.TimeOfDay 7 0 0
  let d4 = T.TimeOfDay 9 0 0

  let d1' = T.TimeOfDay 21 0 0
  let d2' = T.TimeOfDay 23 0 0
  let d3' = T.TimeOfDay 1 0 0
  let d4' = T.TimeOfDay 3 0 0

  {-========================
    時刻の交差を調べるのは、慎重にテストをする場合でもA~Mの13パターンを考えればよい
    ========================
    s1: schedule start
    e1: schedule end
    s2: time slot start
    e2: time slot end
    ========================
     1| 2|  |3 |4
    ========================
    A. True
       s1|  |e1
    s2|        |e2
    B. True
    s1|        |e1
    s2|        |e2
    C. True
       s1|     |e1
    s2|        |e2
    D. True
    s1|     |e1
    s2|        |e2
    E. True
    s1|        |e1
       s2|  |e2
    F. True
    s1|        |e1
       s2|     |e2
    G. True
    s1|        |e1
    s2|     |e2
    H. True
    s1|     |e1
       s2|     |e2
    I. False
    s1|  |e1
       s2|     |e2
    J. False
    s1|  |e1
          s2|  |e2
    K. True
       s1|     |e1
    s2|     |e2
    L. False
       s1|     |e1
    s2|  |e2
    M. False
          s1|  |e1
    s2|  |e2
  -}

  describe "hasTimeRangesIntersect" $ do
    it "Case A." $ do
      D.hasTimeRangesIntersect (d2, d3) (d1, d4) `shouldBe` True
    it "Case B." $ do
      D.hasTimeRangesIntersect (d1, d4) (d1, d4) `shouldBe` True
    it "Case C." $ do
      D.hasTimeRangesIntersect (d2, d4) (d1, d4) `shouldBe` True
    it "Case D." $ do
      D.hasTimeRangesIntersect (d1, d3) (d1, d4) `shouldBe` True
    it "Case E." $ do
      D.hasTimeRangesIntersect (d1, d4) (d2, d3) `shouldBe` True
    it "Case F." $ do
      D.hasTimeRangesIntersect (d1, d4) (d2, d4) `shouldBe` True
    it "Case G." $ do
      D.hasTimeRangesIntersect (d1, d4) (d1, d3) `shouldBe` True
    it "Case H." $ do
      D.hasTimeRangesIntersect (d1, d3) (d2, d4) `shouldBe` True
    it "Case I." $ do
      D.hasTimeRangesIntersect (d1, d2) (d2, d4) `shouldBe` False
    it "Case J." $ do
      D.hasTimeRangesIntersect (d1, d2) (d3, d4) `shouldBe` False
    it "Case K." $ do
      D.hasTimeRangesIntersect (d2, d4) (d1, d3) `shouldBe` True
    it "Case L." $ do
      D.hasTimeRangesIntersect (d2, d4) (d1, d2) `shouldBe` False
    it "Case M." $ do
      D.hasTimeRangesIntersect (d3, d4) (d1, d2) `shouldBe` False

    it "Case A. 日をまたぐ" $ do
      D.hasTimeRangesIntersect (d2', d3') (d1', d4') `shouldBe` True
    it "Case B. 日をまたぐ" $ do
      D.hasTimeRangesIntersect (d1', d4') (d1', d4') `shouldBe` True
    it "Case C. 日をまたぐ" $ do
      D.hasTimeRangesIntersect (d2', d4') (d1', d4') `shouldBe` True
    it "Case D. 日をまたぐ" $ do
      D.hasTimeRangesIntersect (d1', d3') (d1', d4') `shouldBe` True
    it "Case E. 日をまたぐ" $ do
      D.hasTimeRangesIntersect (d1', d4') (d2', d3') `shouldBe` True
    it "Case F. 日をまたぐ" $ do
      D.hasTimeRangesIntersect (d1', d4') (d2', d4') `shouldBe` True
    it "Case G. 日をまたぐ" $ do
      D.hasTimeRangesIntersect (d1', d4') (d1', d3') `shouldBe` True
    it "Case H. 日をまたぐ" $ do
      D.hasTimeRangesIntersect (d1', d3') (d2', d4') `shouldBe` True
    it "Case I. 日をまたぐ" $ do
      D.hasTimeRangesIntersect (d1', d2') (d2', d4') `shouldBe` False
    it "Case J. 日をまたぐ" $ do
      D.hasTimeRangesIntersect (d1', d2') (d3', d4') `shouldBe` False
    it "Case K. 日をまたぐ" $ do
      D.hasTimeRangesIntersect (d2', d4') (d1', d4') `shouldBe` True
    it "Case L. 日をまたぐ" $ do
      D.hasTimeRangesIntersect (d2', d4') (d1', d2') `shouldBe` False
    it "Case M. 日をまたぐ" $ do
      D.hasTimeRangesIntersect (d3', d4') (d1', d2') `shouldBe` False

  describe "timeRangesIntersect" $ do
    it "Case A => [d2, d3)" $ do
      D.timeRangesIntersect (d2, d3) (d1, d4) `shouldBe` Just (d2, d3)
    it "Case B => [d1, d4)" $ do
      D.timeRangesIntersect (d1, d4) (d1, d4) `shouldBe` Just (d1, d4)
    it "Case C => [d2, d4)" $ do
      D.timeRangesIntersect (d2, d4) (d1, d4) `shouldBe` Just (d2, d4)
    it "Case D => [d1, d3)" $ do
      D.timeRangesIntersect (d1, d3) (d1, d4) `shouldBe` Just (d1, d3)
    it "Case E => [d2, d3)" $ do
      D.timeRangesIntersect (d1, d4) (d2, d3) `shouldBe` Just (d2, d3)
    it "Case F => [d2, d4)" $ do
      D.timeRangesIntersect (d1, d4) (d2, d4) `shouldBe` Just (d2, d4)
    it "Case G => [d1, d3)" $ do
      D.timeRangesIntersect (d1, d4) (d1, d3) `shouldBe` Just (d1, d3)
    it "Case H => [d2, d3)" $ do
      D.timeRangesIntersect (d1, d3) (d2, d4) `shouldBe` Just (d2, d3)
    it "Case I => Nothing" $ do
      D.timeRangesIntersect (d1, d2) (d2, d4) `shouldBe` Nothing
    it "Case J => Nothing" $ do
      D.timeRangesIntersect (d1, d2) (d3, d4) `shouldBe` Nothing
    it "Case K => [d2, d3)" $ do
      D.timeRangesIntersect (d2, d4) (d1, d3) `shouldBe` Just (d2, d3)
    it "Case L => Nothing" $ do
      D.timeRangesIntersect (d2, d4) (d1, d2) `shouldBe` Nothing
    it "Case M => Nothing" $ do
      D.timeRangesIntersect (d3, d4) (d1, d2) `shouldBe` Nothing

    it "Case A, 日をまたぐ => [d2', d3')" $ do
      D.timeRangesIntersect (d2', d3') (d1', d4') `shouldBe` Just (d2', d3')
    it "Case B, 日をまたぐ => [d1', d4')" $ do
      D.timeRangesIntersect (d1', d4') (d1', d4') `shouldBe` Just (d1', d4')
    it "Case C, 日をまたぐ => [d2', d4')" $ do
      D.timeRangesIntersect (d2', d4') (d1', d4') `shouldBe` Just (d2', d4')
    it "Case D, 日をまたぐ => [d1', d3')" $ do
      D.timeRangesIntersect (d1', d3') (d1', d4') `shouldBe` Just (d1', d3')
    it "Case E, 日をまたぐ => [d2', d3')" $ do
      D.timeRangesIntersect (d1', d4') (d2', d3') `shouldBe` Just (d2', d3')
    it "Case F, 日をまたぐ => [d2', d4')" $ do
      D.timeRangesIntersect (d1', d4') (d2', d4') `shouldBe` Just (d2', d4')
    it "Case G, 日をまたぐ => [d1', d3')" $ do
      D.timeRangesIntersect (d1', d4') (d1', d3') `shouldBe` Just (d1', d3')
    it "Case H, 日をまたぐ => [d2', d3')" $ do
      D.timeRangesIntersect (d1', d3') (d2', d4') `shouldBe` Just (d2', d3')
    it "Case I, 日をまたぐ => Nothing" $ do
      D.timeRangesIntersect (d1', d2') (d2', d4') `shouldBe` Nothing
    it "Case J, 日をまたぐ => Nothing" $ do
      D.timeRangesIntersect (d1', d2') (d3', d4') `shouldBe` Nothing
    it "Case K, 日をまたぐ => [d2', d3')" $ do
      D.timeRangesIntersect (d2', d4') (d1', d3') `shouldBe` Just (d2', d3')
    it "Case L, 日をまたぐ => Nothing" $ do
      D.timeRangesIntersect (d2', d4') (d1', d2') `shouldBe` Nothing
    it "Case M, 日をまたぐ => Nothing" $ do
      D.timeRangesIntersect (d3', d4') (d1', d2') `shouldBe` Nothing

  describe "intersectTimeRangesWithLocalTime" $ do
    let ld1 = T.fromGregorian 2021 1 1
    let ld2 = T.fromGregorian 2021 1 2
    it "Case A => [d2, d3)" $ do
      D.intersectTimeRangesWithLocalTime (d2, d3) (T.LocalTime ld1 d2, T.LocalTime ld1 d3) `shouldBe` Just (T.LocalTime ld1 d2, T.LocalTime ld1 d3)
    it "Case B => [d1, d4)" $ do
      D.intersectTimeRangesWithLocalTime (d1, d4) (T.LocalTime ld1 d1, T.LocalTime ld1 d4) `shouldBe` Just (T.LocalTime ld1 d1, T.LocalTime ld1 d4)
    it "Case C => [d2, d4)" $ do
      D.intersectTimeRangesWithLocalTime (d2, d4) (T.LocalTime ld1 d2, T.LocalTime ld1 d4) `shouldBe` Just (T.LocalTime ld1 d2, T.LocalTime ld1 d4)
    it "Case D => [d1, d3)" $ do
      D.intersectTimeRangesWithLocalTime (d1, d3) (T.LocalTime ld1 d1, T.LocalTime ld1 d4) `shouldBe` Just (T.LocalTime ld1 d1, T.LocalTime ld1 d3)
    it "Case E => [d2, d3)" $ do
      D.intersectTimeRangesWithLocalTime (d1, d4) (T.LocalTime ld1 d2, T.LocalTime ld1 d3) `shouldBe` Just (T.LocalTime ld1 d2, T.LocalTime ld1 d3)
    it "Case F => [d2, d4)" $ do
      D.intersectTimeRangesWithLocalTime (d1, d4) (T.LocalTime ld1 d2, T.LocalTime ld1 d4) `shouldBe` Just (T.LocalTime ld1 d2, T.LocalTime ld1 d4)
    it "Case G => [d1, d3)" $ do
      D.intersectTimeRangesWithLocalTime (d1, d4) (T.LocalTime ld1 d1, T.LocalTime ld1 d3) `shouldBe` Just (T.LocalTime ld1 d1, T.LocalTime ld1 d3)
    it "Case H => [d2, d3)" $ do
      D.intersectTimeRangesWithLocalTime (d1, d3) (T.LocalTime ld1 d2, T.LocalTime ld1 d4) `shouldBe` Just (T.LocalTime ld1 d2, T.LocalTime ld1 d3)
    it "Case I => Nothing" $ do
      D.intersectTimeRangesWithLocalTime (d1, d2) (T.LocalTime ld1 d2, T.LocalTime ld1 d4) `shouldBe` Nothing
    it "Case J => Nothing" $ do
      D.intersectTimeRangesWithLocalTime (d1, d2) (T.LocalTime ld1 d3, T.LocalTime ld1 d4) `shouldBe` Nothing
    it "Case K => [d2, d3)" $ do
      D.intersectTimeRangesWithLocalTime (d2, d4) (T.LocalTime ld1 d2, T.LocalTime ld1 d3) `shouldBe` Just (T.LocalTime ld1 d2, T.LocalTime ld1 d3)
    it "Case L => Nothing" $ do
      D.intersectTimeRangesWithLocalTime (d2, d4) (T.LocalTime ld1 d1, T.LocalTime ld1 d2) `shouldBe` Nothing
    it "Case M => Nothing" $ do
      D.intersectTimeRangesWithLocalTime (d3, d4) (T.LocalTime ld1 d1, T.LocalTime ld1 d2) `shouldBe` Nothing

    it "Case A, 日をまたぐ => [d2', d3')" $ do
      D.intersectTimeRangesWithLocalTime (d2', d3') (T.LocalTime ld1 d2', T.LocalTime ld1 d3') `shouldBe` Just (T.LocalTime ld1 d2', T.LocalTime ld2 d3')
    it "Case B, 日をまたぐ => [d1', d4')" $ do
      D.intersectTimeRangesWithLocalTime (d1', d4') (T.LocalTime ld1 d1', T.LocalTime ld1 d4') `shouldBe` Just (T.LocalTime ld1 d1', T.LocalTime ld2 d4')
    it "Case C, 日をまたぐ => [d2', d4')" $ do
      D.intersectTimeRangesWithLocalTime (d2', d4') (T.LocalTime ld1 d2', T.LocalTime ld1 d4') `shouldBe` Just (T.LocalTime ld1 d2', T.LocalTime ld2 d4')
    it "Case D, 日をまたぐ => [d1', d3')" $ do
      D.intersectTimeRangesWithLocalTime (d1', d3') (T.LocalTime ld1 d1', T.LocalTime ld1 d4') `shouldBe` Just (T.LocalTime ld1 d1', T.LocalTime ld2 d3')
    it "Case E, 日をまたぐ => [d2', d3')" $ do
      D.intersectTimeRangesWithLocalTime (d1', d4') (T.LocalTime ld1 d2', T.LocalTime ld1 d3') `shouldBe` Just (T.LocalTime ld1 d2', T.LocalTime ld2 d3')
    it "Case F, 日をまたぐ => [d2', d4')" $ do
      D.intersectTimeRangesWithLocalTime (d1', d4') (T.LocalTime ld1 d2', T.LocalTime ld1 d4') `shouldBe` Just (T.LocalTime ld1 d2', T.LocalTime ld2 d4')
    it "Case G, 日をまたぐ => [d1', d3')" $ do
      D.intersectTimeRangesWithLocalTime (d1', d4') (T.LocalTime ld1 d1', T.LocalTime ld1 d3') `shouldBe` Just (T.LocalTime ld1 d1', T.LocalTime ld2 d3')
    it "Case H, 日をまたぐ => [d2', d3')" $ do
      D.intersectTimeRangesWithLocalTime (d1', d3') (T.LocalTime ld1 d2', T.LocalTime ld1 d4') `shouldBe` Just (T.LocalTime ld1 d2', T.LocalTime ld2 d3')
    it "Case I, 日をまたぐ => Nothing" $ do
      D.intersectTimeRangesWithLocalTime (d1', d2') (T.LocalTime ld1 d2', T.LocalTime ld1 d4') `shouldBe` Nothing
    it "Case J, 日をまたぐ => Nothing" $ do
      D.intersectTimeRangesWithLocalTime (d1', d2') (T.LocalTime ld1 d3', T.LocalTime ld1 d4') `shouldBe` Nothing
    it "Case K, 日をまたぐ => [d2', d3')" $ do
      D.intersectTimeRangesWithLocalTime (d2', d4') (T.LocalTime ld1 d2', T.LocalTime ld1 d3') `shouldBe` Just (T.LocalTime ld1 d2', T.LocalTime ld2 d3')
    it "Case L, 日をまたぐ => Nothing" $ do
      D.intersectTimeRangesWithLocalTime (d2', d4') (T.LocalTime ld1 d1', T.LocalTime ld1 d2') `shouldBe` Nothing
    it "Case M, 日をまたぐ => Nothing" $ do
      D.intersectTimeRangesWithLocalTime (d3', d4') (T.LocalTime ld1 d1', T.LocalTime ld1 d2') `shouldBe` Nothing
