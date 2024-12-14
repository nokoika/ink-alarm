module Date (changeTimeZone, timeOfDayFromString, timeZoneFromOffsetString, isWithinTimeRange, isWithinTimeOfDay, hasTimeRangesIntersect) where

import Control.Monad (guard)
import qualified Data.Time as T
import qualified Data.Time.LocalTime as LT
import qualified Text.Read as TR
import Prelude (Bool, Int, Maybe (Just, Nothing), Monad (return), String, not, otherwise, ($), (&&), (*), (+), (<), (<=), (==), (||))

-- UTCTime から ZonedTime に変換
changeTimeZone :: T.UTCTime -> T.TimeZone -> T.ZonedTime
changeTimeZone utcTime timeZone = T.ZonedTime (T.utcToLocalTime timeZone utcTime) timeZone

-- HH:mm形式をTimeOfDayに変換
timeOfDayFromString :: String -> Maybe LT.TimeOfDay
timeOfDayFromString time = case time of
  [h1, h2, ':', m1, m2] -> do
    let hoursStr = [h1, h2]
    let minsStr = [m1, m2]
    hours <- TR.readMaybe hoursStr :: Maybe Int
    mins <- TR.readMaybe minsStr :: Maybe Int
    guard (0 <= hours && hours < 24)
    guard (0 <= mins && mins < 60)
    Just $ LT.TimeOfDay hours mins 0
  _invalidInput -> Nothing

-- タイムゾーン文字列を `TimeZone` 型に変換 (+09:00 -> TimeZone)
timeZoneFromOffsetString :: String -> Maybe T.TimeZone
timeZoneFromOffsetString offset = case offset of
  [signStr, h1, h2, ':', m1, m2] -> do
    let hoursStr = [h1, h2]
    let minsStr = [m1, m2]
    let sign = if signStr == '+' then 1 else -1
    hours <- TR.readMaybe hoursStr :: Maybe Int
    mins <- TR.readMaybe minsStr :: Maybe Int
    guard (0 <= hours && hours <= 14)
    guard (0 <= mins && mins < 60)
    return $ T.minutesToTimeZone $ (hours * 60 + mins) * sign
  ['Z'] -> return T.utc
  _invalidInput -> Nothing

-- TODO: あとで消す
-- 時間が指定の範囲内かどうかを判定。start, endはHH:mm形式(Queryのほう)。localTimeはTZ調整されたLocalTime(SplaApiのほうを調整)のイメージ
-- end は境界を含まない
isWithinTimeRange :: LT.TimeOfDay -> LT.TimeOfDay -> T.LocalTime -> Bool
isWithinTimeRange start end localTime =
  let t = LT.localTimeOfDay localTime
   in if start <= end
        -- 通常の日付内の場合はそのまま範囲チェック
        then start <= t && t < end
        else
          -- start > end の場合、日付またぎ。(ex: 23:00~01:00)
          -- この場合、範囲外となる区間は [end, start) で連続している。
          -- よって、範囲内はその否定、つまり t が [end, start) に入っていない場合。
          not (end <= t && t < start)

-- 時間が区間に含まれるかどうかを判定
-- [start, end) に t が含まれるかどうか
isWithinTimeOfDay :: LT.TimeOfDay -> LT.TimeOfDay -> LT.TimeOfDay -> Bool
isWithinTimeOfDay start end t
  | start <= end = start <= t && t < end
  -- start > end の場合、日付またぎ。(ex: 23:00~01:00)
  -- この場合、範囲外となる区間は [end, start) で連続している。
  -- よって、範囲内はその否定、つまり t が [end, start) に入っていない場合。
  | otherwise = not (end <= t && t < start)

-- 時間が交わるかどうかを判定。A: [s1, e1) と B: [s2, e2) が交わるかどうか
-- ※環状の時刻を扱うため、s1<e2 && s2<e1 は正しくない。環状では`<`を定義できない
hasTimeRangesIntersect :: LT.TimeOfDay -> LT.TimeOfDay -> LT.TimeOfDay -> LT.TimeOfDay -> Bool
hasTimeRangesIntersect s1 e1 s2 e2 = isWithinTimeOfDay s1 e1 s2 || isWithinTimeOfDay s2 e2 s1
