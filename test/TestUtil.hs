module TestUtil (createUTCTime) where

import Data.Time (UTCTime(UTCTime))
import Data.Time.Calendar (fromGregorian)
import Data.Time.Clock (secondsToDiffTime)

import Prelude (Integer, Int, ($), (*), (+))

createUTCTime :: Integer -> Int -> Int -> Integer -> Integer -> UTCTime
createUTCTime y m d h min = UTCTime (fromGregorian y m d) $ secondsToDiffTime (h * 60 * 60 + min * 60)
