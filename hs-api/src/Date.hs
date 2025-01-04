module Date
  ( changeTimeZone,
    timeOfDayFromString,
    timeZoneFromOffsetString,
    timeRangesIntersect,
    intersectTimeRangesWithLocalTime,
    ZonedTimeRange,
    UTCTimeRange,
    LocalTimeRange,
    TimeOfDayRange,
    convertRangedLocalTimeToTimeOfDay,
    convertRangedLocalTimeToUTCTime,
    convertRangedZonedTimeToLocalTime,
    convertRangedUTCTimeToZonedTime,
    convertRangedUTCTimeToLocalTime,
  )
where

import Control.Monad (guard)
import Data.List (sortOn)
import qualified Data.Time as T
import qualified Data.Time.LocalTime as LT
import qualified Text.Read as TR
import Prelude (Foldable (foldr), Int, Integral (mod), Maybe (Just, Nothing), Monad (return), String, compare, divMod, fst, head, init, last, map, max, min, otherwise, tail, ($), (&&), (*), (+), (.), (<), (<=), (==))
import qualified Prelude as P (Ordering (EQ, GT, LT))

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

-- 時間帯の交差部分を求める (日またぎがあるためめっちゃ複雑)
timeRangesIntersect :: TimeOfDayRange -> TimeOfDayRange -> [TimeOfDayRange]
timeRangesIntersect tr1 tr2 =
  let -- (1) TimeOfDay → Int
      mr1 = convertRangedTimeOfDayToMinutes tr1
      mr2 = convertRangedTimeOfDayToMinutes tr2

      -- (2) 日またぎをしている区間を2つの区間に分割
      normalizedRange1 = normalizeRangeMins mr1
      normalizedRange2 = normalizeRangeMins mr2

      -- (3) 交差を求める
      intersections = intersectRangesMins normalizedRange1 normalizedRange2 -- [(Int, Int)] の複数区間

      -- (4) マージ
      merged = mergeOverlappingRanges intersections
   in -- (5) Int → TimeOfDay
      map convertRangedMinutesToTimeOfDayRange merged
  where
    -- (start, end) を日付またぎなら2分割、そうでなければ1区間にする
    normalizeRangeMins :: MinutesRange -> [MinutesRange]
    normalizeRangeMins (start, end) = case start `compare` end of
      P.LT -> [(start, end)]
      P.GT -> [(start, 1440), (0, end)]
      P.EQ -> [(0, 1440)] -- start == end → フル1日扱い

    -- 2つの「正規化済み区間リスト」から交差を求める (start<end のみ対象)
    intersectRangesMins :: [MinutesRange] -> [MinutesRange] -> [MinutesRange]
    intersectRangesMins rs1 rs2 =
      [ (mxStart, mnEnd)
        | (s1, e1) <- rs1,
          (s2, e2) <- rs2,
          let mxStart = max s1 s2
              mnEnd = min e1 e2,
          mxStart < mnEnd
      ]

    -- 区間をマージする
    mergeOverlappingRanges :: [MinutesRange] -> [MinutesRange]
    mergeOverlappingRanges [] = []
    mergeOverlappingRanges [x] = [x]
    mergeOverlappingRanges xs =
      -- 1) start の小さい順にソート
      let sorted = sortOn fst xs -- 例: [(0,60),(1380,1440)] のように並ぶ
      -- 2) fold で上から順にマージ
          merged = foldr step [] sorted
       in -- 3) もし "最後の区間の end=1440" & "最初の区間の start=0" なら例外的に結合
          case merged of
            [] -> []
            [y] -> [y]
            ys ->
              -- マージされているため区間に重複がなく、かつ区間の開始でソートされているため、
              -- 最初と最後だけ確認すれば十分
              -- ※ ys は2つ以上が保証されてるため、exception にはならない
              let (fs, fe) = head ys
                  (ls, le) = last ys
               in if le == 1440 && fs == 0
                    then -- 日付またぎとして結合: (ls, fe)。例えば [ (0,60), ..., (1380,1440) ] => [ (1380,60), ... ]

                      let newRange = (ls, fe)
                          -- 先頭・末尾を除いた「中間」の要素だけ抜き出す
                          -- ※ ys は2つ以上が保証されてるため、exception にはならない
                          mid = tail (init ys)
                       in newRange : mid
                    else ys
      where
        -- foldr step [] で右畳み込み
        step cur [] = [cur]
        step (s1, e1) ((s2, e2) : rest)
          -- 通常の重複・隣接 (例: [10,20) と [20,30) → [10,30))
          | s2 <= e1 = (s1, max e1 e2) : rest
          | otherwise = (s1, e1) : (s2, e2) : rest

-- TimeOfDayとLocalTimeの交差部分をMaybe LocalTimeで返す関数
intersectTimeRangesWithLocalTime ::
  TimeOfDayRange ->
  LocalTimeRange ->
  [LocalTimeRange]
intersectTimeRangesWithLocalTime todRange localRange@(sLocal, _) =
  let todRange' = convertRangedLocalTimeToTimeOfDay localRange
      intersect = timeRangesIntersect todRange todRange'
   in map (toRangedLocalTime sLocal) intersect
  where
    -- timeOfDayをbaseLocalTimeの日付を基準にローカルタイムへ変換する
    toLocalTime :: LT.LocalTime -> LT.TimeOfDay -> LT.LocalTime
    toLocalTime baseLocalTime timeOfDay =
      let baseDay = LT.localDay baseLocalTime
          baseTod = LT.localTimeOfDay baseLocalTime
          -- timeOfDayがbaseLocalTimeより小さい場合は翌日へ
          dayAdjust = if timeOfDay < baseTod then 1 else 0
       in LT.LocalTime (T.addDays dayAdjust baseDay) timeOfDay
    toRangedLocalTime :: LT.LocalTime -> TimeOfDayRange -> LocalTimeRange
    toRangedLocalTime baseLocalTime (startTod, endTod) =
      let startLocal = toLocalTime baseLocalTime startTod
          endLocal = toLocalTime baseLocalTime endTod
       in (startLocal, endLocal)

type ZonedTimeRange = (T.ZonedTime, T.ZonedTime)

type UTCTimeRange = (T.UTCTime, T.UTCTime)

type LocalTimeRange = (LT.LocalTime, LT.LocalTime)

type TimeOfDayRange = (LT.TimeOfDay, LT.TimeOfDay)

type MinutesRange = (Int, Int)

convertRangedLocalTimeToTimeOfDay :: LocalTimeRange -> TimeOfDayRange
convertRangedLocalTimeToTimeOfDay (start, end) = (LT.localTimeOfDay start, LT.localTimeOfDay end)

convertRangedLocalTimeToUTCTime :: T.TimeZone -> LocalTimeRange -> UTCTimeRange
convertRangedLocalTimeToUTCTime timeZone (start, end) = (LT.localTimeToUTC timeZone start, LT.localTimeToUTC timeZone end)

convertRangedZonedTimeToLocalTime :: ZonedTimeRange -> LocalTimeRange
convertRangedZonedTimeToLocalTime (start, end) = (LT.zonedTimeToLocalTime start, LT.zonedTimeToLocalTime end)

convertRangedUTCTimeToZonedTime :: T.TimeZone -> UTCTimeRange -> ZonedTimeRange
convertRangedUTCTimeToZonedTime timeZone (start, end) = (changeTimeZone start timeZone, changeTimeZone end timeZone)

convertRangedUTCTimeToLocalTime :: T.TimeZone -> UTCTimeRange -> LocalTimeRange
convertRangedUTCTimeToLocalTime timeZone = convertRangedZonedTimeToLocalTime . convertRangedUTCTimeToZonedTime timeZone

convertRangedTimeOfDayToMinutes :: TimeOfDayRange -> MinutesRange
convertRangedTimeOfDayToMinutes (start, end) = (todToMinutes start, todToMinutes end)
  where
    todToMinutes :: LT.TimeOfDay -> Int
    todToMinutes t =
      let h = LT.todHour t
          m = LT.todMin t
       in h * 60 + m

convertRangedMinutesToTimeOfDayRange :: MinutesRange -> TimeOfDayRange
convertRangedMinutesToTimeOfDayRange (start, end) = (minutesToTod start, minutesToTod end)
  where
    minutesToTod :: Int -> LT.TimeOfDay
    minutesToTod totalMins =
      let mClamped = totalMins `mod` 1440
          (h, m) = mClamped `divMod` 60
       in LT.TimeOfDay h m 0
