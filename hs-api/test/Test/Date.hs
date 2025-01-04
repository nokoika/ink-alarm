module Test.Date (test) where

import qualified Data.Time as T
import qualified Date as D (changeTimeZone, intersectTimeRangesWithLocalTime, timeOfDayFromString, timeRangesIntersect, timeZoneFromOffsetString)
import Test.Hspec (describe, hspec, it, shouldBe)
import qualified TestUtil as TU
import Prelude (IO, Maybe (Just, Nothing), ($))

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

  describe "timeRangesIntersect" $ do
    it "Case A => [d2, d3)" $ do
      D.timeRangesIntersect (d2, d3) (d1, d4) `shouldBe` [(d2, d3)]
    it "Case B => [d1, d4)" $ do
      D.timeRangesIntersect (d1, d4) (d1, d4) `shouldBe` [(d1, d4)]
    it "Case C => [d2, d4)" $ do
      D.timeRangesIntersect (d2, d4) (d1, d4) `shouldBe` [(d2, d4)]
    it "Case D => [d1, d3)" $ do
      D.timeRangesIntersect (d1, d3) (d1, d4) `shouldBe` [(d1, d3)]
    it "Case E => [d2, d3)" $ do
      D.timeRangesIntersect (d1, d4) (d2, d3) `shouldBe` [(d2, d3)]
    it "Case F => [d2, d4)" $ do
      D.timeRangesIntersect (d1, d4) (d2, d4) `shouldBe` [(d2, d4)]
    it "Case G => [d1, d3)" $ do
      D.timeRangesIntersect (d1, d4) (d1, d3) `shouldBe` [(d1, d3)]
    it "Case H => [d2, d3)" $ do
      D.timeRangesIntersect (d1, d3) (d2, d4) `shouldBe` [(d2, d3)]
    it "Case I => Nothing" $ do
      D.timeRangesIntersect (d1, d2) (d2, d4) `shouldBe` []
    it "Case J => Nothing" $ do
      D.timeRangesIntersect (d1, d2) (d3, d4) `shouldBe` []
    it "Case K => [d2, d3)" $ do
      D.timeRangesIntersect (d2, d4) (d1, d3) `shouldBe` [(d2, d3)]
    it "Case L => Nothing" $ do
      D.timeRangesIntersect (d2, d4) (d1, d2) `shouldBe` []
    it "Case M => Nothing" $ do
      D.timeRangesIntersect (d3, d4) (d1, d2) `shouldBe` []

    it "Case A, 日をまたぐ => [d2', d3')" $ do
      D.timeRangesIntersect (d2', d3') (d1', d4') `shouldBe` [(d2', d3')]
    it "Case B, 日をまたぐ => [d1', d4')" $ do
      D.timeRangesIntersect (d1', d4') (d1', d4') `shouldBe` [(d1', d4')]
    it "Case C, 日をまたぐ => [d2', d4')" $ do
      D.timeRangesIntersect (d2', d4') (d1', d4') `shouldBe` [(d2', d4')]
    it "Case D, 日をまたぐ => [d1', d3')" $ do
      D.timeRangesIntersect (d1', d3') (d1', d4') `shouldBe` [(d1', d3')]
    it "Case E, 日をまたぐ => [d2', d3')" $ do
      D.timeRangesIntersect (d1', d4') (d2', d3') `shouldBe` [(d2', d3')]
    it "Case F, 日をまたぐ => [d2', d4')" $ do
      D.timeRangesIntersect (d1', d4') (d2', d4') `shouldBe` [(d2', d4')]
    it "Case G, 日をまたぐ => [d1', d3')" $ do
      D.timeRangesIntersect (d1', d4') (d1', d3') `shouldBe` [(d1', d3')]
    it "Case H, 日をまたぐ => [d2', d3')" $ do
      D.timeRangesIntersect (d1', d3') (d2', d4') `shouldBe` [(d2', d3')]
    it "Case I, 日をまたぐ => Nothing" $ do
      D.timeRangesIntersect (d1', d2') (d2', d4') `shouldBe` []
    it "Case J, 日をまたぐ => Nothing" $ do
      D.timeRangesIntersect (d1', d2') (d3', d4') `shouldBe` []
    it "Case K, 日をまたぐ => [d2', d3')" $ do
      D.timeRangesIntersect (d2', d4') (d1', d3') `shouldBe` [(d2', d3')]
    it "Case L, 日をまたぐ => Nothing" $ do
      D.timeRangesIntersect (d2', d4') (d1', d2') `shouldBe` []
    it "Case M, 日をまたぐ => Nothing" $ do
      D.timeRangesIntersect (d3', d4') (d1', d2') `shouldBe` []

    it "00:00~23:30 と 23:00~01:00 の場合、23:00~23:30 と 00:00~01:00 になる" $ do
      D.timeRangesIntersect (T.TimeOfDay 0 0 0, T.TimeOfDay 23 30 0) (T.TimeOfDay 23 0 0, T.TimeOfDay 1 0 0) `shouldBe` [(T.TimeOfDay 0 0 0, T.TimeOfDay 1 0 0), (T.TimeOfDay 23 0 0, T.TimeOfDay 23 30 0)]

    it "00:00-00:00 である場合は常にマッチする" $ do
      D.timeRangesIntersect (T.TimeOfDay 0 0 0, T.TimeOfDay 0 0 0) (T.TimeOfDay 0 0 0, T.TimeOfDay 2 0 0) `shouldBe` [(T.TimeOfDay 0 0 0, T.TimeOfDay 2 0 0)]
      D.timeRangesIntersect (T.TimeOfDay 0 0 0, T.TimeOfDay 0 0 0) (T.TimeOfDay 2 0 0, T.TimeOfDay 4 0 0) `shouldBe` [(T.TimeOfDay 2 0 0, T.TimeOfDay 4 0 0)]
      D.timeRangesIntersect (T.TimeOfDay 0 0 0, T.TimeOfDay 0 0 0) (T.TimeOfDay 23 0 0, T.TimeOfDay 1 0 0) `shouldBe` [(T.TimeOfDay 23 0 0, T.TimeOfDay 1 0 0)]
      D.timeRangesIntersect (T.TimeOfDay 0 0 0, T.TimeOfDay 0 0 0) (T.TimeOfDay 22 0 0, T.TimeOfDay 0 0 0) `shouldBe` [(T.TimeOfDay 22 0 0, T.TimeOfDay 0 0 0)]

  describe "intersectTimeRangesWithLocalTime" $ do
    let ld1 = T.fromGregorian 2021 1 1
    let ld2 = T.fromGregorian 2021 1 2

    it "Case A => [d2, d3)" $ do
      D.intersectTimeRangesWithLocalTime (d2, d3) (T.LocalTime ld1 d1, T.LocalTime ld1 d4) `shouldBe` [(T.LocalTime ld1 d2, T.LocalTime ld1 d3)]
    it "Case B => [d1, d4)" $ do
      D.intersectTimeRangesWithLocalTime (d1, d4) (T.LocalTime ld1 d1, T.LocalTime ld1 d4) `shouldBe` [(T.LocalTime ld1 d1, T.LocalTime ld1 d4)]
    it "Case C => [d2, d4)" $ do
      D.intersectTimeRangesWithLocalTime (d2, d4) (T.LocalTime ld1 d1, T.LocalTime ld1 d4) `shouldBe` [(T.LocalTime ld1 d2, T.LocalTime ld1 d4)]
    it "Case D => [d1, d3)" $ do
      D.intersectTimeRangesWithLocalTime (d1, d3) (T.LocalTime ld1 d1, T.LocalTime ld1 d4) `shouldBe` [(T.LocalTime ld1 d1, T.LocalTime ld1 d3)]
    it "Case E => [d2, d3)" $ do
      D.intersectTimeRangesWithLocalTime (d1, d4) (T.LocalTime ld1 d2, T.LocalTime ld1 d3) `shouldBe` [(T.LocalTime ld1 d2, T.LocalTime ld1 d3)]
    it "Case F => [d2, d4)" $ do
      D.intersectTimeRangesWithLocalTime (d1, d4) (T.LocalTime ld1 d2, T.LocalTime ld1 d4) `shouldBe` [(T.LocalTime ld1 d2, T.LocalTime ld1 d4)]
    it "Case G => [d1, d3)" $ do
      D.intersectTimeRangesWithLocalTime (d1, d4) (T.LocalTime ld1 d1, T.LocalTime ld1 d3) `shouldBe` [(T.LocalTime ld1 d1, T.LocalTime ld1 d3)]
    it "Case H => [d2, d3)" $ do
      D.intersectTimeRangesWithLocalTime (d1, d3) (T.LocalTime ld1 d2, T.LocalTime ld1 d4) `shouldBe` [(T.LocalTime ld1 d2, T.LocalTime ld1 d3)]
    it "Case I => Nothing" $ do
      D.intersectTimeRangesWithLocalTime (d1, d2) (T.LocalTime ld1 d2, T.LocalTime ld1 d4) `shouldBe` []
    it "Case J => Nothing" $ do
      D.intersectTimeRangesWithLocalTime (d1, d2) (T.LocalTime ld1 d3, T.LocalTime ld1 d4) `shouldBe` []
    it "Case K => [d2, d3)" $ do
      D.intersectTimeRangesWithLocalTime (d2, d4) (T.LocalTime ld1 d1, T.LocalTime ld1 d3) `shouldBe` [(T.LocalTime ld1 d2, T.LocalTime ld1 d3)]
    it "Case L => Nothing" $ do
      D.intersectTimeRangesWithLocalTime (d2, d4) (T.LocalTime ld1 d1, T.LocalTime ld1 d2) `shouldBe` []
    it "Case M => Nothing" $ do
      D.intersectTimeRangesWithLocalTime (d3, d4) (T.LocalTime ld1 d1, T.LocalTime ld1 d2) `shouldBe` []

    it "Case A, 日をまたぐ => [d2', d3')" $ do
      D.intersectTimeRangesWithLocalTime (d2', d3') (T.LocalTime ld1 d1', T.LocalTime ld2 d4') `shouldBe` [(T.LocalTime ld1 d2', T.LocalTime ld2 d3')]
    it "Case B, 日をまたぐ => [d1', d4')" $ do
      D.intersectTimeRangesWithLocalTime (d1', d4') (T.LocalTime ld1 d1', T.LocalTime ld2 d4') `shouldBe` [(T.LocalTime ld1 d1', T.LocalTime ld2 d4')]
    it "Case C, 日をまたぐ => [d2', d4')" $ do
      D.intersectTimeRangesWithLocalTime (d2', d4') (T.LocalTime ld1 d1', T.LocalTime ld2 d4') `shouldBe` [(T.LocalTime ld1 d2', T.LocalTime ld2 d4')]
    it "Case D, 日をまたぐ => [d1', d3')" $ do
      D.intersectTimeRangesWithLocalTime (d1', d3') (T.LocalTime ld1 d1', T.LocalTime ld2 d4') `shouldBe` [(T.LocalTime ld1 d1', T.LocalTime ld2 d3')]
    it "Case E, 日をまたぐ => [d2', d3')" $ do
      D.intersectTimeRangesWithLocalTime (d1', d4') (T.LocalTime ld1 d2', T.LocalTime ld2 d3') `shouldBe` [(T.LocalTime ld1 d2', T.LocalTime ld2 d3')]
    it "Case F, 日をまたぐ => [d2', d4')" $ do
      D.intersectTimeRangesWithLocalTime (d1', d4') (T.LocalTime ld1 d2', T.LocalTime ld2 d4') `shouldBe` [(T.LocalTime ld1 d2', T.LocalTime ld2 d4')]
    it "Case G, 日をまたぐ => [d1', d3')" $ do
      D.intersectTimeRangesWithLocalTime (d1', d4') (T.LocalTime ld1 d1', T.LocalTime ld2 d3') `shouldBe` [(T.LocalTime ld1 d1', T.LocalTime ld2 d3')]
    it "Case H, 日をまたぐ => [d2', d3')" $ do
      D.intersectTimeRangesWithLocalTime (d1', d3') (T.LocalTime ld1 d2', T.LocalTime ld2 d4') `shouldBe` [(T.LocalTime ld1 d2', T.LocalTime ld2 d3')]
    it "Case I, 日をまたぐ => Nothing" $ do
      D.intersectTimeRangesWithLocalTime (d1', d2') (T.LocalTime ld1 d2', T.LocalTime ld2 d4') `shouldBe` []
    it "Case J, 日をまたぐ => Nothing" $ do
      D.intersectTimeRangesWithLocalTime (d1', d2') (T.LocalTime ld1 d3', T.LocalTime ld2 d4') `shouldBe` []
    it "Case K, 日をまたぐ => [d2', d3')" $ do
      D.intersectTimeRangesWithLocalTime (d2', d4') (T.LocalTime ld1 d1', T.LocalTime ld2 d3') `shouldBe` [(T.LocalTime ld1 d2', T.LocalTime ld2 d3')]
    it "Case L, 日をまたぐ => Nothing" $ do
      D.intersectTimeRangesWithLocalTime (d2', d4') (T.LocalTime ld1 d1', T.LocalTime ld2 d2') `shouldBe` []
    it "Case M, 日をまたぐ => Nothing" $ do
      D.intersectTimeRangesWithLocalTime (d3', d4') (T.LocalTime ld1 d1', T.LocalTime ld2 d2') `shouldBe` []

    it "00:00~00:00 である場合は常にマッチする" $ do
      D.intersectTimeRangesWithLocalTime (T.TimeOfDay 0 0 0, T.TimeOfDay 0 0 0) (T.LocalTime ld1 (T.TimeOfDay 2 0 0), T.LocalTime ld1 (T.TimeOfDay 4 0 0)) `shouldBe` [(T.LocalTime ld1 (T.TimeOfDay 2 0 0), T.LocalTime ld1 (T.TimeOfDay 4 0 0))]
      D.intersectTimeRangesWithLocalTime (T.TimeOfDay 0 0 0, T.TimeOfDay 0 0 0) (T.LocalTime ld1 (T.TimeOfDay 23 0 0), T.LocalTime ld1 (T.TimeOfDay 1 0 0)) `shouldBe` [(T.LocalTime ld1 (T.TimeOfDay 23 0 0), T.LocalTime ld2 (T.TimeOfDay 1 0 0))]

