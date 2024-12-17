module Filter.Internal.Schedule
  ( filterDefaultSchedule,
    filterEventMatch,
    inRules,
    inStage,
    inTimeSlot,
    inTimeSlots,
  )
where

import qualified Data.Maybe as M
import qualified Data.Time as T
import qualified Date
import qualified Query as Q
import qualified SplaApi as S
import Prelude (Bool (False, True), Maybe (Just, Nothing), and, const, elem, fst, map, not, or, ($), (&&), (.), (<$>), (==))

maybeTrue :: (a -> Bool) -> Maybe a -> Bool
maybeTrue = M.maybe True

-- API のスケジュールが timeSlot に該当するかどうかを返す
-- 1. スケジュールの時刻が TimeSlot の時刻と交差しているかどうか
-- 2. 交差の開始時刻の曜日が TimeSlot の曜日と一致するかどうか
-- 判定のタイムゾーンは utcOffset で指定されたものを使う
inTimeSlot :: T.UTCTime -> T.UTCTime -> T.TimeZone -> Q.TimeSlot -> Bool
inTimeSlot apiStartTime apiEndTime utcOffset Q.TimeSlot {start, end, dayOfWeek} =
  M.isJust intersect && matchDayOfWeek
  where
    pickApiLocalTime :: T.UTCTime -> T.LocalTime
    pickApiLocalTime utcTime = T.zonedTimeToLocalTime $ Date.changeTimeZone utcTime utcOffset
    pickTimeSlotTimeOfDay :: Q.TimeSlotTimeOfDay -> T.TimeOfDay
    pickTimeSlotTimeOfDay (Q.TimeSlotTimeOfDay timeOfDay) = timeOfDay
    intersect = Date.intersectTimeRangesWithLocalTime (pickTimeSlotTimeOfDay start) (pickTimeSlotTimeOfDay end) (pickApiLocalTime apiStartTime) (pickApiLocalTime apiEndTime)

    -- 交差部分がある場合、交差の開始時刻の曜日がTimeSlotの曜日と一致するかどうかをチェックする
    intersectStartTime = fst <$> intersect
    localTimeToWeekDay = T.dayOfWeek . T.localDay
    getDayOfWeek :: Q.TimeSlotDayOfWeek -> T.DayOfWeek
    getDayOfWeek (Q.TimeSlotDayOfWeek dow) = dow
    sameDayOfWeek :: T.DayOfWeek -> Bool
    sameDayOfWeek = case intersectStartTime of
      Just i -> (== localTimeToWeekDay i)
      Nothing -> const False
    matchDayOfWeek = maybeTrue (sameDayOfWeek . getDayOfWeek) dayOfWeek

inTimeSlots :: T.UTCTime -> T.UTCTime -> T.TimeZone -> [Q.TimeSlot] -> Bool
inTimeSlots apiStartTime apiEndTime utcOffset timeSlots = or [inTimeSlot apiStartTime apiEndTime utcOffset timeSlot | timeSlot <- timeSlots]

inStage :: [S.Stage] -> Q.StageFilter -> Bool
inStage apiStages Q.StageFilter {matchBothStages, stageIds} =
  match [apiStageId `elem` stageIds | apiStageId <- [id | S.Stage {id} <- apiStages]]
  where
    match = if matchBothStages then and else or

inRules :: S.Rule -> [Q.Rule] -> Bool
inRules S.Rule {key = apiRuleKey} rules = apiRuleKey `elem` ruleKeys
  where
    ruleKeys = map S.convertQueryRule rules

filterDefaultSchedule :: Q.FilterCondition -> S.DefaultSchedule -> T.TimeZone -> Q.MatchType -> Bool
filterDefaultSchedule Q.FilterCondition {matchType, stages, rules, timeSlots} S.DefaultSchedule {startTime = apiStartTime, endTime = apiEndTime, rule = apiRule, stages = apiStages, isFest = apiIsFest} utcOffset apiMatchType =
  and
    [ not apiIsFest, -- フェスの場合はデフォルトスケジュールのルールで遊ぶことができない
      matchType == apiMatchType, -- マッチタイプ(オープンかXマッチか等)が一致するか
      maybeTrue (inTimeSlots apiStartTime apiEndTime utcOffset) timeSlots, -- 時間帯が通知設定にかぶっているか。未指定の場合は任意の時間でマッチ
      maybeTrue (inMaybeStages apiStages) stages, -- 選んだステージがスケジュールに含まれているか。未指定の場合は任意のステージでマッチ
      maybeTrue (inMaybeRules apiRule) rules -- ルールが指定したものに含まれるか。未指定の場合は任意のルールでマッチ
    ]
  where
    inMaybeStages :: Maybe [S.Stage] -> Q.StageFilter -> Bool
    inMaybeStages apiStages' selectedStages = maybeTrue (`inStage` selectedStages) apiStages'
    inMaybeRules :: Maybe S.Rule -> [Q.Rule] -> Bool
    inMaybeRules apiRule' selectedRules = maybeTrue (`inRules` selectedRules) apiRule'

filterEventMatch :: Q.FilterCondition -> S.EventMatch -> T.TimeZone -> Bool
filterEventMatch Q.FilterCondition {stages, rules, timeSlots, matchType} S.EventMatch {startTime = apiStartTime, endTime = apiEndTime, rule = apiRule, stages = apiStages, isFest = apiIsFest} utcOffset =
  and
    [ not apiIsFest, -- フェスの場合にイベントマッチが来ることはない
      matchType == Q.Event, -- マッチタイプがイベントであること
      maybeTrue (inTimeSlots apiStartTime apiEndTime utcOffset) timeSlots, -- 時間帯が通知設定にかぶっているか。未指定の場合は任意の時間でマッチ
      maybeTrue (inStage apiStages) stages, -- 選んだステージがスケジュールに含まれているか。未指定の場合は任意のステージでマッチ
      maybeTrue (inRules apiRule) rules -- ルールが指定したものに含まれるか。未指定の場合は任意のルールでマッチ
    ]
