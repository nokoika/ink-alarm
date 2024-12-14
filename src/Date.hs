module Date (changeTimeZone, timeOfDayFromString, timeZoneFromOffsetString, isWithinTimeRange) where

import Control.Monad (guard)
import qualified Data.Time as T
import qualified Data.Time.LocalTime as LT
import qualified Text.Read as TR
import Prelude (Bool, Int, Maybe (Just, Nothing), Monad (return), String, not, ($), (&&), (*), (+), (<), (<=), (==))

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
