module Filter
  ( createIcalInput,
    createICalEventsFromDefaultSchedules,
    createICalEventsFromEventMatches,
    defaultReminders,
    filterDefaultSchedule,
    filterEventMatch,
    inRules,
    inStage,
    inTimeSlot,
    inTimeSlots,
  )
where

import qualified Data.Maybe as M
import qualified Data.Time as T
import qualified Date (changeTimeZone, hasTimeRangesIntersect, timeRangesIntersect)
import Debug.Trace (trace)
import qualified ICal as I
import qualified Query as Q
import qualified SplaApi as S
import Prelude (Bool (True), Maybe, Show (show), and, elem, map, not, or, ($), (&&), (*), (++), (.), (==), (||))

maybeTrue :: (a -> Bool) -> Maybe a -> Bool
maybeTrue = M.maybe True

-- API のスケジュールが timeSlot に該当するかどうかを返す
-- 1. apiStartTime or apiEndTime のどちらかが、isWithinTimeRange に含まれるかどうか
-- 2. apiStartTime の曜日が TimeSlot の曜日と一致するかどうか
-- 判定のタイムゾーンは utcOffsetTimeZone で指定されたものを使う
inTimeSlot :: T.UTCTime -> T.UTCTime -> Q.UtcOffsetTimeZone -> Q.TimeSlot -> Bool
inTimeSlot apiStartTime apiEndTime utcOffsetTimeZone Q.TimeSlot {start, end, dayOfWeek} =
  matchDayOfWeek
  where
    Q.UtcOffsetTimeZone timeZone = utcOffsetTimeZone
    pickApiTimeOfDay :: T.UTCTime -> T.TimeOfDay
    pickApiTimeOfDay utcTime = T.localTimeOfDay $ T.utcToLocalTime timeZone utcTime
    pickTimeSlotTimeOfDay :: Q.TimeSlotTimeOfDay -> T.TimeOfDay
    pickTimeSlotTimeOfDay (Q.TimeSlotTimeOfDay timeOfDay) = timeOfDay

    intersect = Date.timeRangesIntersect (pickTimeSlotTimeOfDay start) (pickTimeSlotTimeOfDay end) (pickApiTimeOfDay apiStartTime) (pickApiTimeOfDay apiEndTime)

    localStartTime = T.zonedTimeToLocalTime $ Date.changeTimeZone apiStartTime timeZone
    localTimeToWeekDay = T.dayOfWeek . T.localDay
    getDayOfWeek :: Q.TimeSlotDayOfWeek -> T.DayOfWeek
    getDayOfWeek (Q.TimeSlotDayOfWeek dow) = dow
    sameDayOfWeek :: T.DayOfWeek -> Bool
    sameDayOfWeek = (== localTimeToWeekDay localStartTime)
    matchDayOfWeek = maybeTrue (sameDayOfWeek . getDayOfWeek) dayOfWeek

inTimeSlots :: T.UTCTime -> T.UTCTime -> Q.UtcOffsetTimeZone -> [Q.TimeSlot] -> Bool
inTimeSlots apiStartTime apiEndTime utcOffset timeSlots = or [inTimeSlot apiStartTime apiEndTime utcOffset timeSlot | timeSlot <- timeSlots]

inStage :: [S.Stage] -> Q.StageFilter -> Bool
inStage apiStages Q.StageFilter {matchBothStages, stageIds} =
  match [apiStageId `elem` stageIds | apiStageId <- [id | S.Stage {id} <- apiStages]]
  where
    match = if matchBothStages then and else or

-- TODO: 実際はapiRuleKeyはキーなので変換する
inRules :: S.Rule -> [Q.Rule] -> Bool
inRules S.Rule {key = apiRuleKey} rules = apiRuleKey `elem` ruleKeys
  where
    ruleKeys = map S.convertQueryRule rules

filterDefaultSchedule :: Q.FilterCondition -> S.DefaultSchedule -> Q.UtcOffsetTimeZone -> Q.MatchType -> Bool
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

filterEventMatch :: Q.FilterCondition -> S.EventMatch -> Q.UtcOffsetTimeZone -> Bool
filterEventMatch Q.FilterCondition {stages, rules, timeSlots} S.EventMatch {startTime = apiStartTime, endTime = apiEndTime, rule = apiRule, stages = apiStages, isFest = apiIsFest} utcOffset =
  and
    [ not apiIsFest, -- フェスの場合にイベントマッチが来ることはない
      maybeTrue (inTimeSlots apiStartTime apiEndTime utcOffset) timeSlots, -- 時間帯が通知設定にかぶっているか。未指定の場合は任意の時間でマッチ
      maybeTrue (inStage apiStages) stages, -- 選んだステージがスケジュールに含まれているか。未指定の場合は任意のステージでマッチ
      maybeTrue (inRules apiRule) rules -- ルールが指定したものに含まれるか。未指定の場合は任意のルールでマッチ
    ]

convertNotificationsToReminders :: [Q.NotificationSetting] -> [I.Reminder]
convertNotificationsToReminders notifications =
  [ I.Reminder {I.trigger = I.ReminderTrigger {I.time = minutesBefore * 60}, I.action = I.Display}
    | Q.NotificationSetting {Q.minutesBefore} <- notifications
  ]

defaultReminders :: [I.Reminder]
defaultReminders =
  [ I.Reminder {I.trigger = I.ReminderTrigger {I.time = 30 * 60}, I.action = I.Display},
    I.Reminder {I.trigger = I.ReminderTrigger {I.time = 60 * 60}, I.action = I.Email}
  ]

createICalEventsFromDefaultSchedules :: Q.QueryRoot -> [S.DefaultSchedule] -> Q.MatchType -> [I.ICalEvent]
createICalEventsFromDefaultSchedules queryRoot defaultSchedules matchType =
  [ I.ICalEvent
      { I.summary = "さまりー",
        I.description = "せつめい",
        I.start = startTime, -- TODO: startTime を UTCTime にして渡す
        I.end = endTime, -- TODO: startTime を UTCTime にして渡す
        I.reminders = convertNotificationsToReminders (M.fromMaybe [] notifications)
      }
    | defaultSchedule <- defaultSchedules,
      let S.DefaultSchedule {S.startTime, S.endTime} = defaultSchedule
          Q.QueryRoot {Q.utcOffset, Q.filters} = queryRoot,
      filter <- filters,
      let Q.FilterCondition {notifications} = filter,
      filterDefaultSchedule filter defaultSchedule utcOffset matchType
  ]

createICalEventsFromEventMatches :: Q.QueryRoot -> [S.EventMatch] -> [I.ICalEvent]
createICalEventsFromEventMatches queryRoot eventMatches =
  [ I.ICalEvent
      { I.summary = "さまりー",
        I.description = "せつめい",
        I.start = startTime, -- TODO: ZonedTime を渡す
        I.end = endTime, -- TODO: ZonedTime を渡す
        I.reminders = convertNotificationsToReminders (M.fromMaybe [] notifications)
      }
    | eventMatch <- eventMatches,
      let S.EventMatch {S.startTime, S.endTime} = eventMatch
          Q.QueryRoot {Q.utcOffset, Q.filters} = queryRoot,
      filter <- filters,
      let Q.FilterCondition {notifications} = filter,
      filterEventMatch filter eventMatch utcOffset
  ]

createIcalInput :: Q.QueryRoot -> S.Result -> I.ICalInput
createIcalInput queryRoot result =
  I.ICalInput
    { I.language = Q.language queryRoot,
      I.events = regular ++ bankaraChallenge ++ bankaraOpen ++ x ++ event
    }
  where
    regular = createICalEventsFromDefaultSchedules queryRoot (S.regular result) Q.Regular
    bankaraChallenge = createICalEventsFromDefaultSchedules queryRoot (S.bankaraChallenge result) Q.BankaraChallenge
    bankaraOpen = createICalEventsFromDefaultSchedules queryRoot (S.bankaraOpen result) Q.BankaraOpen
    x = createICalEventsFromDefaultSchedules queryRoot (S.x result) Q.XMatch
    event = createICalEventsFromEventMatches queryRoot (S.event result)
