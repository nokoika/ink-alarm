module TestUtil (createUTCTime, 
createLocalTime, createTimeZone, createZonedTime) where

import Data.Time (LocalTime (LocalTime), TimeOfDay (TimeOfDay), TimeZone (TimeZone), UTCTime (UTCTime), ZonedTime (ZonedTime))
import Data.Time.Calendar (fromGregorian)
import Data.Time.Clock (secondsToDiffTime)
import Prelude (Bool (..), Float, Int, Integer, String, round, ($), (*), (+))

createUTCTime :: Integer -> Int -> Int -> Integer -> Integer -> UTCTime
createUTCTime y m d h min = UTCTime (fromGregorian y m d) $ secondsToDiffTime (h * 60 * 60 + min * 60)

createLocalTime :: Integer -> Int -> Int -> Int -> Int -> LocalTime
createLocalTime y m d h min = LocalTime (fromGregorian y m d) (TimeOfDay h min 0)

createTimeZone :: Float -> String -> TimeZone
createTimeZone hour = TimeZone (round (hour * 60)) False

createZonedTime :: (Integer, Int, Int, Int, Int) -> (Float, String) -> ZonedTime
createZonedTime (y, m, d, h, min) (hour, zone) = ZonedTime (createLocalTime y m d h min) (createTimeZone hour zone)
