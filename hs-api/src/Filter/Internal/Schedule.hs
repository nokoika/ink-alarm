module Filter.Internal.Schedule
  ( inRules,
    inStage,
    timeSlotIntersection,
    timeSlotsIntersection,
    getMatchedTimeRangesFromDefaultSchedule,
    getMatchedTimeRangesFromEventMatch,
  )
where

import qualified Data.Maybe as M
import qualified Data.Time as T
import qualified Date
import qualified Query as Q
import qualified SplaApi as S
import Prelude (Bool (False, True), Maybe (Just, Nothing), and, const, elem, fst, map, not, or, (.), (<$>), (==), any)

maybeTrue :: (a -> Bool) -> Maybe a -> Bool
maybeTrue = M.maybe True

-- API のスケジュールが timeSlot に該当するかどうかを返す
-- 1. スケジュールの時刻が TimeSlot の時刻と交差しているかどうか
-- 2. 交差の開始時刻の曜日が TimeSlot の曜日と一致するかどうか
-- 判定のタイムゾーンは utcOffset で指定されたものを使う
timeSlotIntersection :: Date.UTCTimeRange -> T.TimeZone -> Q.TimeSlot -> Maybe Date.LocalTimeRange
timeSlotIntersection utcRange utcOffset timeSlot@Q.TimeSlot {dayOfWeeks} =
  if matchDayOfWeek
    then intersect
    else Nothing
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
    inDayOfWeek :: [Q.TimeSlotDayOfWeek] -> Bool
    inDayOfWeek = any (sameDayOfWeek . getDayOfWeek)
    matchDayOfWeek = maybeTrue inDayOfWeek dayOfWeeks

timeSlotsIntersection :: Date.UTCTimeRange -> T.TimeZone -> [Q.TimeSlot] -> [Date.LocalTimeRange]
timeSlotsIntersection utcRange utcOffset timeSlots = M.catMaybes [timeSlotIntersection utcRange utcOffset timeSlot | timeSlot <- timeSlots]

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

inMode :: Q.Mode -> [Q.Mode] -> Bool
inMode apiMode' selectedModes = apiMode' `elem` selectedModes

getMatchedTimeRangesFromDefaultSchedule :: Q.FilterCondition -> S.DefaultSchedule -> T.TimeZone -> Q.Mode -> [Date.UTCTimeRange]
getMatchedTimeRangesFromDefaultSchedule Q.FilterCondition {modes, stages, rules, timeSlots} S.DefaultSchedule {startTime = apiStartTime, endTime = apiEndTime, rule = apiRule, stages = apiStages, isFest = apiIsFest} utcOffset apiMode =
  [ Date.convertRangedLocalTimeToUTCTime utcOffset intersection
    | not apiIsFest,
      maybeTrue (inMode apiMode) modes,
      maybeTrue (inMaybeStages apiStages) stages,
      maybeTrue (inMaybeRules apiRule) rules,
      let intersections :: [Date.LocalTimeRange] = case timeSlots of
            Just ts -> timeSlotsIntersection (apiStartTime, apiEndTime) utcOffset ts
            Nothing -> [Date.convertRangedUTCTimeToLocalTime utcOffset (apiStartTime, apiEndTime)],
      intersection <- intersections
  ]
  where
    inMaybeStages :: Maybe [S.Stage] -> Q.StageFilter -> Bool
    inMaybeStages apiStages' selectedStages = maybeTrue (`inStage` selectedStages) apiStages'
    inMaybeRules :: Maybe S.Rule -> [Q.Rule] -> Bool
    inMaybeRules apiRule' selectedRules = maybeTrue (`inRules` selectedRules) apiRule'

getMatchedTimeRangesFromEventMatch :: Q.FilterCondition -> S.EventMatch -> T.TimeZone -> [Date.UTCTimeRange]
getMatchedTimeRangesFromEventMatch Q.FilterCondition {modes, stages, rules, timeSlots} S.EventMatch {S.startTime = apiStartTime, S.endTime = apiEndTime, S.rule = apiRule, S.stages = apiStages, isFest = apiIsFest} utcOffset =
  [ Date.convertRangedLocalTimeToUTCTime utcOffset intersection
    | not apiIsFest,
      maybeTrue (inMode Q.Event) modes,
      maybeTrue (inStage apiStages) stages,
      maybeTrue (inRules apiRule) rules,
      let intersections :: [Date.LocalTimeRange] = case timeSlots of
            Just ts -> timeSlotsIntersection (apiStartTime, apiEndTime) utcOffset ts
            Nothing -> [Date.convertRangedUTCTimeToLocalTime utcOffset (apiStartTime, apiEndTime)],
      intersection <- intersections
  ]
