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
import Prelude (Bool (False, True), Maybe (Just, Nothing), and, const, elem, fst, map, not, or, (&&), (.), (<$>), (==))

maybeTrue :: (a -> Bool) -> Maybe a -> Bool
maybeTrue = M.maybe True

-- API のスケジュールが timeSlot に該当するかどうかを返す
-- 1. スケジュールの時刻が TimeSlot の時刻と交差しているかどうか
-- 2. 交差の開始時刻の曜日が TimeSlot の曜日と一致するかどうか
-- 判定のタイムゾーンは utcOffset で指定されたものを使う
inTimeSlot :: Date.UTCTimeRange -> T.TimeZone -> Q.TimeSlot -> Bool
inTimeSlot utcRange utcOffset timeSlot@Q.TimeSlot {dayOfWeek} =
  M.isJust intersect && matchDayOfWeek
  where
    intersect = Date.intersectTimeRangesWithLocalTime (Q.convertTimeSlotToTimeOfDayRange timeSlot) (Date.convertRangedUTCTimeToLocalTime utcOffset utcRange)

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

inTimeSlots :: Date.UTCTimeRange -> T.TimeZone -> [Q.TimeSlot] -> Bool
inTimeSlots utcRange utcOffset timeSlots = or [inTimeSlot utcRange utcOffset timeSlot | timeSlot <- timeSlots]

inStage :: [S.Stage] -> Q.StageFilter -> Bool
inStage apiStages Q.StageFilter {matchBothStages, stageIds} =
  match [apiStageId `elem` stageIds | apiStageId <- [id | S.Stage {id} <- apiStages]]
  where
    match = if matchBothStages then and else or

inRules :: S.Rule -> [Q.Rule] -> Bool
inRules S.Rule {key = apiRuleKey} rules = apiRuleKey `elem` ruleKeys
  where
    ruleKeys = map convertRule rules
    convertRule :: Q.Rule -> S.RuleKey
    convertRule Q.TurfWar = S.TurfWar
    convertRule Q.SplatZones = S.SplatZones
    convertRule Q.TowerControl = S.TowerControl
    convertRule Q.Rainmaker = S.Rainmaker
    convertRule Q.ClamBlitz = S.ClamBlitz

filterDefaultSchedule :: Q.FilterCondition -> S.DefaultSchedule -> T.TimeZone -> Q.Mode -> Bool
filterDefaultSchedule Q.FilterCondition {mode, stages, rules, timeSlots} S.DefaultSchedule {startTime = apiStartTime, endTime = apiEndTime, rule = apiRule, stages = apiStages, isFest = apiIsFest} utcOffset apiMode =
  and
    [ not apiIsFest, -- フェスの場合はデフォルトスケジュールのルールで遊ぶことができない
      mode == apiMode, -- モード(オープンかXマッチか等)が一致するか
      maybeTrue (inTimeSlots (apiStartTime, apiEndTime) utcOffset) timeSlots, -- 時間帯が通知設定にかぶっているか。未指定の場合は任意の時間でマッチ
      maybeTrue (inMaybeStages apiStages) stages, -- 選んだステージがスケジュールに含まれているか。未指定の場合は任意のステージでマッチ
      maybeTrue (inMaybeRules apiRule) rules -- ルールが指定したものに含まれるか。未指定の場合は任意のルールでマッチ
    ]
  where
    inMaybeStages :: Maybe [S.Stage] -> Q.StageFilter -> Bool
    inMaybeStages apiStages' selectedStages = maybeTrue (`inStage` selectedStages) apiStages'
    inMaybeRules :: Maybe S.Rule -> [Q.Rule] -> Bool
    inMaybeRules apiRule' selectedRules = maybeTrue (`inRules` selectedRules) apiRule'

filterEventMatch :: Q.FilterCondition -> S.EventMatch -> T.TimeZone -> Bool
filterEventMatch Q.FilterCondition {stages, rules, timeSlots, mode} S.EventMatch {startTime = apiStartTime, endTime = apiEndTime, rule = apiRule, stages = apiStages, isFest = apiIsFest} utcOffset =
  and
    [ not apiIsFest, -- フェスの場合にイベントマッチが来ることはない
      mode == Q.Event, -- モードがイベントであること
      maybeTrue (inTimeSlots (apiStartTime, apiEndTime) utcOffset) timeSlots, -- 時間帯が通知設定にかぶっているか。未指定の場合は任意の時間でマッチ
      maybeTrue (inStage apiStages) stages, -- 選んだステージがスケジュールに含まれているか。未指定の場合は任意のステージでマッチ
      maybeTrue (inRules apiRule) rules -- ルールが指定したものに含まれるか。未指定の場合は任意のルールでマッチ
    ]
