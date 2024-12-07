module TestUtil (createUTCTime, createLocalTime, createTimeZone) where

import Data.Time (UTCTime(UTCTime), LocalTime (LocalTime), TimeOfDay (TimeOfDay), TimeZone(TimeZone))
import Data.Time.Calendar (fromGregorian)
import Data.Time.Clock (secondsToDiffTime)

import Prelude (Integer, Int, ($), (*), (+), String, Bool (..), Float, round)

createUTCTime :: Integer -> Int -> Int -> Integer -> Integer -> UTCTime
createUTCTime y m d h min = UTCTime (fromGregorian y m d) $ secondsToDiffTime (h * 60 * 60 + min * 60)

createLocalTime :: Integer -> Int -> Int -> Int -> Int -> LocalTime
createLocalTime y m d h min = LocalTime (fromGregorian y m d) (TimeOfDay h min 0)

createTimeZone :: Float -> String -> TimeZone
createTimeZone hour = TimeZone (round (hour * 60)) False
