module Filter () where

import Control.Monad (when)
import qualified Data.Maybe as M (fromJust, fromMaybe, isJust, isNothing, maybe)
import qualified Data.Time as D
import qualified Data.Time.Format.ISO8601 as DF (iso8601ParseM)
import qualified Data.Time.LocalTime as LT (LocalTime, TimeOfDay (..), localTimeOfDay)
import qualified ICal as I (ICalEvent (..), ICalInput (..), Reminder (..), ReminderAction (..), ReminderTrigger (..), buildICalText)
import Query (StageFilter (StageFilter))
import qualified Query as Q (FilterCondition (..), NotificationSetting (..), QueryRoot (..), StageFilter (..), TimeSlot (..))
import SplaApi (EventMatch (isFest))
import qualified SplaApi as S (DefaultSchedule (..), EventMatch (..), EventSummary (..), Result (..), Root (..), Rule (..), Stage (..), fetchSchedule)
import qualified Text.Read as TR (readMaybe)
import Prelude (($), (&&), (*), (+), (++), (/=), (<$>), (<=), (==), (||))
import qualified Prelude as P (Bool (..), Foldable (null), Int, Maybe (..), Monad (return), Show (show), String, and, concatMap, drop, elem, not, or, otherwise, read, take)

maybeTrue :: (a -> P.Bool) -> P.Maybe a -> P.Bool
maybeTrue = M.maybe P.True

-- | タイムゾーン文字列を `TimeZone` 型に変換 (+09:00 -> TimeZone)
timeZoneFromOffsetString :: P.String -> P.Maybe D.TimeZone
timeZoneFromOffsetString offset = do
  hours <- TR.readMaybe (P.take 3 offset) :: P.Maybe P.Int
  mins <- TR.readMaybe (P.drop 4 offset) :: P.Maybe P.Int
  P.return $ D.minutesToTimeZone (hours * 60 + mins)

-- 時間が指定の範囲内かどうかを判定。start, endはHH:mm形式(Queryのほう)。localTimeはTZ調整されたDateTime(SplaApiのほうを調整)のイメージ
isWithinTimeRange :: LT.TimeOfDay -> LT.TimeOfDay -> D.ZonedTime -> P.Bool
isWithinTimeRange start end localTime =
  let localTimeOfDay = LT.localTimeOfDay $ D.zonedTimeToLocalTime localTime
   in start <= localTimeOfDay && localTimeOfDay <= end

-- | HH:mm形式をTimeOfDayに変換
timeOfDayFromString :: P.String -> P.Maybe LT.TimeOfDay
timeOfDayFromString time = do
  hours <- TR.readMaybe (P.take 2 time) :: P.Maybe P.Int
  mins <- TR.readMaybe (P.drop 3 time) :: P.Maybe P.Int
  P.return $ LT.TimeOfDay hours mins 0

-- | ISO8601形式を解釈し、タイムゾーンを変更
-- | offset は +09:00 のような文字列を解釈したタイムゾーン
-- | time は 2021-01-01T00:00:00 のような文字列
parseISO8601WithTimeZone :: D.TimeZone -> P.String -> P.Maybe D.ZonedTime
parseISO8601WithTimeZone offset time = do
  (`D.ZonedTime` offset) <$> DF.iso8601ParseM time

-- apiStartTime or apiEndTime のどちらかが、isWithinTimeRange に含まれるかどうかを返す
-- isWithinTimeRange の第3引数は apiStartTime と apiEndTime から作るんだが、タイムゾーンはQ.utcOffsetを適用する
-- TODO: 曜日、fromJust削除
inTimeSlot :: P.String -> P.String -> P.String -> Q.TimeSlot -> P.Bool
inTimeSlot apiStartTime apiEndTime utcOffset Q.TimeSlot {start, end, dayOfWeek} =
  let startTime = M.fromJust $ timeOfDayFromString start
      endTime = M.fromJust $ timeOfDayFromString end
      timeZone = M.fromJust $ timeZoneFromOffsetString utcOffset
      localStartTime = M.fromJust $ parseISO8601WithTimeZone timeZone apiStartTime
      localEndTime = M.fromJust $ parseISO8601WithTimeZone timeZone apiEndTime
   in isWithinTimeRange startTime endTime localStartTime || isWithinTimeRange startTime endTime localEndTime

inTimeSlots :: P.String -> P.String -> P.String -> [Q.TimeSlot] -> P.Bool
inTimeSlots apiStartTime apiEndTime utcOffset timeSlots = P.or [inTimeSlot apiStartTime apiEndTime utcOffset timeSlot | timeSlot <- timeSlots]

inStage :: [S.Stage] -> Q.StageFilter -> P.Bool
inStage apiStages Q.StageFilter {matchBothStages, stageIds} =
  match [apiStageId `P.elem` stageIds | apiStageId <- [id | S.Stage {id} <- apiStages]]
  where
    match = if matchBothStages then P.and else P.or

-- TODO: 実際はapiRuleKeyはキーなので変換する
inRules :: S.Rule -> [P.String] -> P.Bool
inRules S.Rule {key = apiRuleKey} rules = apiRuleKey `P.elem` rules

-- | スケジュール単体とquery条件を渡して、マッチするかどうかを返す
-- | 最初は雑に書いてあとでリファクタリングする
filterMultipleDefaultSchedule :: Q.QueryRoot -> S.DefaultSchedule -> P.String -> P.Bool
filterMultipleDefaultSchedule Q.QueryRoot {filters, utcOffset} defaultSchedule apiMatchType =
  P.or [filterDefaultSchedule filter defaultSchedule utcOffset apiMatchType | filter <- filters]

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

-- filterMultipleEventMatch :: Q.QueryRoot -> S.EventMatch -> P.Bool
-- filterMultipleEventMatch Q.QueryRoot {filters, utcOffset} S.EventMatch {startTime = apiStartTime, endTime = apiEndTime, rule = apiRule, stages = apiStages, isFest = apiIsFest} =
--   P.or -- どれかのquery条件にマッチするかどうか
--     [ P.and
--         [ P.not apiIsFest, -- フェスの場合にイベントマッチが来ることはない
--           maybeTrue (inTimeSlots apiStartTime apiEndTime utcOffset) timeSlots, -- 時間帯が通知設定にかぶっているか。未指定の場合は任意の時間でマッチ
--           maybeTrue (inStage apiStages) stages, -- 選んだステージがスケジュールに含まれているか。未指定の場合は任意のステージでマッチ
--           maybeTrue (inRules apiRule) rules -- ルールが指定したものに含まれるか。未指定の場合は任意のルールでマッチ
--         ]
--       | Q.FilterCondition {stages, rules, timeSlots} <- filters
--     ]

filterMultipleEventMatch :: Q.QueryRoot -> S.EventMatch -> P.Bool
filterMultipleEventMatch Q.QueryRoot {filters, utcOffset} eventMatch =
  P.or [filterEventMatch filter eventMatch utcOffset | filter <- filters]

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
        I.start = startTime, -- TODO: ZonedTime を渡す
        I.end = endTime, -- TODO: ZonedTime を渡す
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

-- q: このhogeって関数名じゃなくて、具体的にどういう名前にするといいですか？
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
