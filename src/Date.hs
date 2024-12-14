module Date (changeTimeZone, timeOfDayFromString, timeZoneFromOffsetString, isWithinTimeRange, isWithinTimeOfDay, hasTimeRangesIntersect, timeRangesIntersect, intersectTimeRangesWithLocalTime) where

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

-- 2つの時間区間の交差部分をMaybeで返す関数
-- 区間はいずれも [start, end) 形式
timeRangesIntersect :: LT.TimeOfDay -> LT.TimeOfDay -> LT.TimeOfDay -> LT.TimeOfDay -> Maybe (LT.TimeOfDay, LT.TimeOfDay)
timeRangesIntersect s1 e1 s2 e2
  | not (hasTimeRangesIntersect s1 e1 s2 e2) = Nothing
  | otherwise = Just (iStart, iEnd)
  where
    -- 交差している場合、開始点はs1またはs2のどちらかがもう一方の区間に含まれている。
    iStart
      | isWithinTimeOfDay s2 e2 s1 = s1
      | otherwise = s2

    -- 終了点を決める:
    -- iStartから見て、e1とe2のどちらが先に範囲外になるか比較する。
    -- isWithinTimeOfDay iStart eX eY = True の場合、
    -- iStartから出発して eY より先に eX が来ることを意味する。
    -- ここでやりたいのは、「iStartから見たとき、どちらが手前にある終端か」を決めること。

    iEnd
      | isFirstEnd e1 e2 = e1
      | otherwise = e2

    -- iStartから見て、e1がe2より先にくる（つまりiStart->e1->e2の順序）かどうかを判定するヘルパー関数
    isFirstEnd x y = isWithinTimeOfDay iStart y x


-- TimeOfDayとLocalTimeの交差部分をMaybe LocalTimeで返す関数
intersectTimeRangesWithLocalTime ::
  LT.TimeOfDay ->
  LT.TimeOfDay ->
  LT.LocalTime ->
  LT.LocalTime ->
  Maybe (LT.LocalTime, LT.LocalTime)
intersectTimeRangesWithLocalTime s1 e1 s2Local e2Local =
  case timeRangesIntersect s1 e1 (LT.localTimeOfDay s2Local) (LT.localTimeOfDay e2Local) of
    Nothing -> Nothing
    Just (iStartTod, iEndTod) ->
      let iStartLocal = toLocalTime s2Local iStartTod
          iEndLocal = toLocalTimeAdjust iStartLocal iEndTod
       in Just (iStartLocal, iEndLocal)
  where
    -- iStartTodをs2Localの日付を基準にローカルタイムへ変換する
    -- もしiStartTodがs2LocalのTimeOfDayよりも小さい場合、翌日にずらす（と解釈）
    toLocalTime :: LT.LocalTime -> LT.TimeOfDay -> LT.LocalTime
    toLocalTime baseLocal iTod =
      let baseDay = LT.localDay baseLocal
          baseTod = LT.localTimeOfDay baseLocal
          dayAdjust = if iTod < baseTod then 1 else 0
       in LT.LocalTime (T.addDays dayAdjust baseDay) iTod

    -- 終了時刻は開始時刻を基準に整合をとる。
    toLocalTimeAdjust :: LT.LocalTime -> LT.TimeOfDay -> LT.LocalTime
    toLocalTimeAdjust startLocal iEndTod =
      let startDay = LT.localDay startLocal
          startTod = LT.localTimeOfDay startLocal
          -- iEndTodがiStartTodより小さい場合は翌日へ
          dayAdjust = if iEndTod < startTod then 1 else 0
       in LT.LocalTime (T.addDays dayAdjust startDay) iEndTod
