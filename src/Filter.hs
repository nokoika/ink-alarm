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
    isWithinTimeRange,
    changeTimeZone,
    timeZoneFromOffsetString,
    timeOfDayFromString,
  )
where

import qualified Data.Maybe as M (fromJust, fromMaybe, maybe)
import qualified Data.Time as D
import qualified Data.Time.LocalTime as LT (TimeOfDay (..), localTimeOfDay)
import qualified ICal as I (ICalEvent (..), ICalInput (..), Reminder (..), ReminderAction (..), ReminderTrigger (..))
import qualified Query as Q (FilterCondition (..), NotificationSetting (..), QueryRoot (..), StageFilter (..), TimeSlot (..))
import SplaApi (EventMatch (isFest))
import qualified SplaApi as S (DefaultSchedule (..), EventMatch (..), EventSummary (..), Result (..), Root (..), Rule (..), Stage (..), fetchSchedule)
import qualified Text.Read as TR (readMaybe)
import Prelude (($), (&&), (*), (+), (++), (<=), (<), (==), (||))
import qualified Prelude as P (Bool (..), Foldable (length, null), Int, Maybe (..), Monad (return), Show (show), String, and, concatMap, drop, elem, not, or, otherwise, read, splitAt, take)
import Data.Time (UTCTime)

maybeTrue :: (a -> P.Bool) -> P.Maybe a -> P.Bool
maybeTrue = M.maybe P.True

-- タイムゾーン文字列を `TimeZone` 型に変換 (+09:00 -> TimeZone)
timeZoneFromOffsetString :: P.String -> P.Maybe D.TimeZone
timeZoneFromOffsetString offset = case offset of
  [signStr, h1, h2, ':', m1, m2] -> do
    let hoursStr = [h1, h2]
    let minsStr = [m1, m2]
    let sign = if signStr == '+' then 1 else -1
    hours <- TR.readMaybe hoursStr :: P.Maybe P.Int
    mins <- TR.readMaybe minsStr :: P.Maybe P.Int
    P.return $ D.minutesToTimeZone $ (hours * 60 + mins) * sign
  ['Z'] -> P.return D.utc
  _invalidInput -> P.Nothing

-- 時間が指定の範囲内かどうかを判定。start, endはHH:mm形式(Queryのほう)。localTimeはTZ調整されたDateTime(SplaApiのほうを調整)のイメージ
-- end は境界を含まない
isWithinTimeRange :: LT.TimeOfDay -> LT.TimeOfDay -> D.ZonedTime -> P.Bool
isWithinTimeRange start end localTime =
  let localTimeOfDay = LT.localTimeOfDay $ D.zonedTimeToLocalTime localTime
   in start <= localTimeOfDay && localTimeOfDay < end

-- HH:mm形式をTimeOfDayに変換
timeOfDayFromString :: P.String -> P.Maybe LT.TimeOfDay
timeOfDayFromString time = case time of
  [h1, h2, ':', m1, m2] -> do
    let hoursStr = [h1, h2]
    let minsStr = [m1, m2]
    hours <- TR.readMaybe hoursStr :: P.Maybe P.Int
    mins <- TR.readMaybe minsStr :: P.Maybe P.Int
    P.Just $ LT.TimeOfDay hours mins 0
  _invalidInput -> P.Nothing

-- UTCTime から ZonedTime に変換
changeTimeZone :: UTCTime -> D.TimeZone -> D.ZonedTime
changeTimeZone utcTime timeZone = D.ZonedTime (D.utcToLocalTime timeZone utcTime) timeZone

-- apiStartTime or apiEndTime のどちらかが、isWithinTimeRange に含まれるかどうかを返す
-- isWithinTimeRange の第3引数は apiStartTime と apiEndTime から作るんだが、タイムゾーンはQ.utcOffsetを適用する
-- TODO: 曜日対応、fromJust削除
inTimeSlot :: UTCTime -> UTCTime -> P.String -> Q.TimeSlot -> P.Bool
inTimeSlot apiStartTime apiEndTime utcOffset Q.TimeSlot {start, end, dayOfWeek} =
  let startTime = M.fromJust $ timeOfDayFromString start
      endTime = M.fromJust $ timeOfDayFromString end
      timeZone = M.fromJust $ timeZoneFromOffsetString utcOffset
      localStartTime = changeTimeZone apiStartTime timeZone
      localEndTime = changeTimeZone apiEndTime timeZone
   in isWithinTimeRange startTime endTime localStartTime || isWithinTimeRange startTime endTime localEndTime

inTimeSlots :: UTCTime -> UTCTime -> P.String -> [Q.TimeSlot] -> P.Bool
inTimeSlots apiStartTime apiEndTime utcOffset timeSlots = P.or [inTimeSlot apiStartTime apiEndTime utcOffset timeSlot | timeSlot <- timeSlots]

inStage :: [S.Stage] -> Q.StageFilter -> P.Bool
inStage apiStages Q.StageFilter {matchBothStages, stageIds} =
  match [apiStageId `P.elem` stageIds | apiStageId <- [id | S.Stage {id} <- apiStages]]
  where
    match = if matchBothStages then P.and else P.or

-- TODO: 実際はapiRuleKeyはキーなので変換する
inRules :: S.Rule -> [P.String] -> P.Bool
inRules S.Rule {key = apiRuleKey} rules = apiRuleKey `P.elem` rules

filterDefaultSchedule :: Q.FilterCondition -> S.DefaultSchedule -> P.String -> P.String -> P.Bool
filterDefaultSchedule Q.FilterCondition {matchType, stages, rules, timeSlots} S.DefaultSchedule {startTime = apiStartTime, endTime = apiEndTime, rule = apiRule, stages = apiStages, isFest = apiIsFest} utcOffset apiMatchType =
  P.and
    [ P.not apiIsFest, -- フェスの場合はデフォルトスケジュールのルールで遊ぶことができない
      matchType == apiMatchType, -- マッチタイプ(オープンかXマッチか等)が一致するか
      maybeTrue (inTimeSlots apiStartTime apiEndTime utcOffset) timeSlots, -- 時間帯が通知設定にかぶっているか。未指定の場合は任意の時間でマッチ
      maybeTrue (inMaybeStages apiStages) stages, -- 選んだステージがスケジュールに含まれているか。未指定の場合は任意のステージでマッチ
      maybeTrue (inMaybeRules apiRule) rules -- ルールが指定したものに含まれるか。未指定の場合は任意のルールでマッチ
    ]
  where
    inMaybeStages :: P.Maybe [S.Stage] -> Q.StageFilter -> P.Bool
    inMaybeStages apiStages' selectedStages = maybeTrue (`inStage` selectedStages) apiStages'
    inMaybeRules :: P.Maybe S.Rule -> [P.String] -> P.Bool
    inMaybeRules apiRule' selectedRules = maybeTrue (`inRules` selectedRules) apiRule'

filterEventMatch :: Q.FilterCondition -> S.EventMatch -> P.String -> P.Bool
filterEventMatch Q.FilterCondition {stages, rules, timeSlots} S.EventMatch {startTime = apiStartTime, endTime = apiEndTime, rule = apiRule, stages = apiStages, isFest = apiIsFest} utcOffset =
  P.and
    [ P.not apiIsFest, -- フェスの場合にイベントマッチが来ることはない
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

createICalEventsFromDefaultSchedules :: Q.QueryRoot -> [S.DefaultSchedule] -> P.String -> [I.ICalEvent]
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
    regular = createICalEventsFromDefaultSchedules queryRoot (S.regular result) "regular"
    bankaraChallenge = createICalEventsFromDefaultSchedules queryRoot (S.bankaraChallenge result) "bankara_challenge"
    bankaraOpen = createICalEventsFromDefaultSchedules queryRoot (S.bankaraOpen result) "bankara_open"
    x = createICalEventsFromDefaultSchedules queryRoot (S.x result) "x"
    event = createICalEventsFromEventMatches queryRoot (S.event result)
