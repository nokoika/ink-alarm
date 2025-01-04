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
import qualified Data.Time as T
import qualified Data.Time.LocalTime as LT
import qualified Text.Read as TR
import Prelude (Int, Maybe (Just, Nothing), Monad (return), String, otherwise, ($), (&&), (*), (+), (.), (<), (<=), (==), max, min, map, divMod, Foldable (foldr), fst, Integral (mod), compare, head, tail, init, error)
import qualified Prelude as P (Ordering (GT, LT, EQ))
import Data.List (sortOn)

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
timeRangesIntersect (start1, end1) (start2, end2) =
  let
      -- (1) TimeOfDay → Int
      s1 = todToMinutes start1
      e1 = todToMinutes end1
      s2 = todToMinutes start2
      e2 = todToMinutes end2

      -- (2) normalizeRangeMins
      nr1 = normalizeRangeMins (s1, e1)
      nr2 = normalizeRangeMins (s2, e2)

      -- (3) 交差
      intr = intersectRangesMins nr1 nr2  -- [(Int, Int)] の複数区間

      -- (4) マージ
      merged = mergeOverlappingRanges intr

      -- (5) Int → TimeOfDay
  in
      map (\(st, en) -> (minutesToTod st, minutesToTod en)) merged
  where

  -- TimeOfDay <-> 分(Int) 変換
  todToMinutes :: LT.TimeOfDay -> Int
  todToMinutes t =
    let h = LT.todHour t
        m = LT.todMin t
    in h * 60 + m

  -- 一旦 0～1440 の範囲に納めて TimeOfDay を作る。
  -- 「start > end」(翌日またぎ) はあくまで (Int,Int) の組で表す想定なので、
  -- ここでは 0 <= x < 1440 にクランプして TimeOfDay を作るだけにする。
  minutesToTod :: Int -> LT.TimeOfDay
  minutesToTod totalMins =
    let mClamped = totalMins `mod` 1440  -- 0～1440 のあいだ
        (h, m)   = mClamped `divMod` 60
    in LT.TimeOfDay h m 0

  -- (start, end) を日付またぎなら2分割、そうでなければ1区間にする
  normalizeRangeMins :: (Int, Int) -> [(Int, Int)]
  normalizeRangeMins (start, end) = case start `compare` end of
    P.LT -> [ (start, end) ]
    P.GT -> [ (start, 1440), (0, end) ]
    P.EQ -> [ (0, 1440) ]  -- start == end → フル1日扱い

  -- 2つの「正規化済み区間リスト」から交差を求める (start<end のみ対象)
  intersectRangesMins :: [(Int, Int)] -> [(Int, Int)] -> [(Int, Int)]
  intersectRangesMins rs1 rs2 =
    [ (mxStart, mnEnd)
      | (s1, e1) <- rs1
      , (s2, e2) <- rs2
      , let mxStart = max s1 s2
            mnEnd   = min e1 e2
      , mxStart < mnEnd
    ]

  -- 区間をマージする
  mergeOverlappingRanges :: [(Int, Int)] -> [(Int, Int)]
  mergeOverlappingRanges []  = []
  mergeOverlappingRanges [x] = [x]
  mergeOverlappingRanges xs  =
    -- 1) start の小さい順にソート
    let sorted = sortOn fst xs   -- 例: [(0,60),(1380,1440)] のように並ぶ
        -- 2) fold で上から順にマージ
        merged = foldr step [] sorted
        -- 3) もし "最後の区間の end=1440" & "最初の区間の start=0" なら例外的に結合
    in case merged of
         [] -> []
         [y] -> [y]
         ys  ->
           let (fs, fe) = head ys
               (ls, le)  = last' ys
           in if le == 1440 && fs == 0
                then 
                  -- 日付またぎとして結合: (ls, fe)
                  -- 例えば [ (0,60), ..., (1380,1440) ] => [ (1380,60), ... ]
                  let newRange = (ls, fe)
                      -- 先頭・末尾を除いた「中間」の要素だけ抜き出す
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

      -- 自前で last を安全に取り出す補助
      last' :: [a] -> a
      last' [z] = z
      last' (_:zs) = last' zs
      last' [] = error "last': empty list" -- 起こらない前提

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
