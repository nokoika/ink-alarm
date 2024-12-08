module Date (changeTimeZone, timeOfDayFromString, timeZoneFromOffsetString, isWithinTimeRange) where

import Data.Time (UTCTime)
import qualified Data.Time as D
import qualified Data.Time.LocalTime as LT (TimeOfDay (..), localTimeOfDay)
import Prelude (($), (&&), (*), (+), (++), (<=), (<), (==), (||))
import qualified Prelude as P (Bool (..), Foldable (length, null), Int, Maybe (..), Monad (return), Show (show), String, and, concatMap, drop, elem, not, or, otherwise, read, splitAt, take)
import qualified Text.Read as TR (readMaybe)
import qualified Control.Monad as P

-- UTCTime から ZonedTime に変換
changeTimeZone :: UTCTime -> D.TimeZone -> D.ZonedTime
changeTimeZone utcTime timeZone = D.ZonedTime (D.utcToLocalTime timeZone utcTime) timeZone

-- HH:mm形式をTimeOfDayに変換
timeOfDayFromString :: P.String -> P.Maybe LT.TimeOfDay
timeOfDayFromString time = case time of
  [h1, h2, ':', m1, m2] -> do
    let hoursStr = [h1, h2]
    let minsStr = [m1, m2]
    hours <- TR.readMaybe hoursStr :: P.Maybe P.Int
    mins <- TR.readMaybe minsStr :: P.Maybe P.Int
    P.guard (0 <= hours && hours < 24)
    P.guard (0 <= mins && mins < 60)
    P.Just $ LT.TimeOfDay hours mins 0
  _invalidInput -> P.Nothing

-- タイムゾーン文字列を `TimeZone` 型に変換 (+09:00 -> TimeZone)
timeZoneFromOffsetString :: P.String -> P.Maybe D.TimeZone
timeZoneFromOffsetString offset = case offset of
  [signStr, h1, h2, ':', m1, m2] -> do
    let hoursStr = [h1, h2]
    let minsStr = [m1, m2]
    let sign = if signStr == '+' then 1 else -1
    hours <- TR.readMaybe hoursStr :: P.Maybe P.Int
    mins <- TR.readMaybe minsStr :: P.Maybe P.Int
    P.guard (0 <= hours && hours <= 14)
    P.guard (0 <= mins && mins < 60)
    P.return $ D.minutesToTimeZone $ (hours * 60 + mins) * sign
  ['Z'] -> P.return D.utc
  _invalidInput -> P.Nothing

-- 時間が指定の範囲内かどうかを判定。start, endはHH:mm形式(Queryのほう)。localTimeはTZ調整されたDateTime(SplaApiのほうを調整)のイメージ
-- end は境界を含まない
isWithinTimeRange :: LT.TimeOfDay -> LT.TimeOfDay -> D.ZonedTime -> P.Bool
isWithinTimeRange start end localTime =
  let localTimeOfDay = LT.localTimeOfDay $ D.zonedTimeToLocalTime localTime
   in start <= localTimeOfDay && localTimeOfDay < end

