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
import qualified Date (changeTimeZone, intersectTimeRangesWithLocalTime)
import qualified ICal as I
import qualified Query as Q
import qualified SplaApi as S
import Prelude (Bool (False, True), Maybe (Just, Nothing), and, const, elem, fst, map, not, or, ($), (&&), (*), (++), (.), (<$>), (==))

maybeTrue :: (a -> Bool) -> Maybe a -> Bool
maybeTrue = M.maybe True

-- API のスケジュールが timeSlot に該当するかどうかを返す
-- 1. スケジュールの時刻が TimeSlot の時刻と交差しているかどうか
-- 2. 交差の開始時刻の曜日が TimeSlot の曜日と一致するかどうか
-- 判定のタイムゾーンは utcOffsetTimeZone で指定されたものを使う
inTimeSlot :: T.UTCTime -> T.UTCTime -> Q.UtcOffsetTimeZone -> Q.TimeSlot -> Bool
inTimeSlot apiStartTime apiEndTime utcOffsetTimeZone Q.TimeSlot {start, end, dayOfWeek} =
  M.isJust intersect && matchDayOfWeek
  where
    Q.UtcOffsetTimeZone timeZone = utcOffsetTimeZone
    pickApiLocalTime :: T.UTCTime -> T.LocalTime
    pickApiLocalTime utcTime = T.zonedTimeToLocalTime $ Date.changeTimeZone utcTime timeZone
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

inTimeSlots :: T.UTCTime -> T.UTCTime -> Q.UtcOffsetTimeZone -> [Q.TimeSlot] -> Bool
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
filterEventMatch Q.FilterCondition {stages, rules, timeSlots, matchType} S.EventMatch {startTime = apiStartTime, endTime = apiEndTime, rule = apiRule, stages = apiStages, isFest = apiIsFest} utcOffset =
  and
    [ not apiIsFest, -- フェスの場合にイベントマッチが来ることはない
      matchType == Q.Event, -- マッチタイプがイベントであること
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

-- buildSummary :: Q.Language -> Q.MatchType -> S.Rule -> [S.Stage] -> String
-- buildSummary language matchType S.Rule {name} stages = name ++ " " ++ stageNames
--   where
--     getStageName S.Stage {name} = name
--     stageNames = concatMap (\stage -> getStageName stage ++ " ") stages
--     stageIds = map S.id stages
--     stageIdAndNames = map (\stage -> (S.id stage, S.name stage)) stages


createICalEventsFromDefaultSchedules :: Q.QueryRoot -> [S.DefaultSchedule] -> Q.MatchType -> [I.ICalEvent]
createICalEventsFromDefaultSchedules queryRoot defaultSchedules matchType =
  [ I.ICalEvent
      { I.summary = "さまりー",
        I.description = "せつめい",
        I.start = startTime,
        I.end = endTime,
        I.reminders = convertNotificationsToReminders (M.fromMaybe [] notifications)
      }
    | defaultSchedule <- defaultSchedules,
      let S.DefaultSchedule {startTime, endTime} = defaultSchedule
          Q.QueryRoot {utcOffset, filters} = queryRoot,
      filter <- filters,
      let Q.FilterCondition {notifications} = filter,
      filterDefaultSchedule filter defaultSchedule utcOffset matchType
  ]

createICalEventsFromEventMatches :: Q.QueryRoot -> [S.EventMatch] -> [I.ICalEvent]
createICalEventsFromEventMatches queryRoot eventMatches =
  [ I.ICalEvent
      { I.summary = "さまりー",
        I.description = "せつめい",
        I.start = startTime,
        I.end = endTime,
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
