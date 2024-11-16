module Lib (generateICalEvents, someFunc) where

import qualified ICal (ICalEvent (..), Reminder (..), ReminderAction (..), ReminderTimeUnit (..), ReminderTrigger (..), buildICalText)
import Prelude (Bool (False), Eq, IO, Int, Maybe (Just), Show, String, ($))
import qualified Prelude as P (putStrLn, return)

-- SplatoonStageScheduleQueryを表すデータ型
data SplatoonStageScheduleQuery = SplatoonStageScheduleQuery
  { language :: String, -- "ja" | "en"
    filters :: [FilterCondition]
  }
  deriving (Show, Eq)

-- フィルタ条件
data FilterCondition
  = MatchTypeFilter
  { matchType :: String, -- "bankara_open" | "bankara_challenge" | "x" | "regular" | "event"
    stages :: Maybe StageFilter,
    rules :: Maybe [String], -- ["TURF_WAR", "AREA", ...]
    timeSlots :: Maybe [TimeSlot],
    notifications :: Maybe [NotificationSetting]
  }
  deriving (Show, Eq)

-- ステージフィルタ
data StageFilter = StageFilter
  { matchBothStages :: Bool,
    stageIds :: [Int]
  }
  deriving (Show, Eq)

-- 時間帯
data TimeSlot = TimeSlot
  { start :: String, -- HH:mm
    end :: String, -- HH:mm
    utcOffset :: Maybe String, -- e.g., "+09:00"
    dayOfWeek :: Maybe String -- e.g., "Monday"
  }
  deriving (Show, Eq)

-- 通知設定
newtype NotificationSetting = NotificationSetting
  { minutesBefore :: Int
  }
  deriving (Show, Eq)

-- 入力クエリを受け取り、HTTPリクエストを行い、ICalEventのリストを返す
generateICalEvents ::
  SplatoonStageScheduleQuery -> -- 入力クエリ
  IO [ICal.ICalEvent] -- HTTPリクエストを行い、ICalEventのリストを返す
generateICalEvents query = do
  -- ここにHTTPリクエストを行う処理を書く
  -- 仮にダミーのデータを返す
  P.return
    [ ICal.ICalEvent
        { summary = "スプラトゥーン2 レギュラーマッチ",
          description = "スプラトゥーン2のレギュラーマッチが開催されます。",
          start = "2020-01-01T00:00:00Z",
          end = "2020-01-01T01:00:00Z",
          url = Just "https://example.com",
          reminders =
            [ ICal.Reminder
                { trigger = ICal.ReminderTrigger {time = 15, unit = ICal.Minute},
                  action = ICal.Display
                },
              ICal.Reminder
                { trigger = ICal.ReminderTrigger {time = 1, unit = ICal.Hour},
                  action = ICal.Email
                }
            ]
        },
      ICal.ICalEvent
        { summary = "スプラトゥーン2 ガチエリア",
          description = "スプラトゥーン2のガチエリアが開催されます。",
          start = "2020-01-01T01:00:00Z",
          end = "2020-01-01T02:00:00Z",
          url = Just "https://example.com",
          reminders =
            [ ICal.Reminder
                { trigger = ICal.ReminderTrigger {time = 10, unit = ICal.Minute},
                  action = ICal.Display
                },
              ICal.Reminder
                { trigger = ICal.ReminderTrigger {time = 30, unit = ICal.Minute},
                  action = ICal.Display
                }
            ]
        }
    ]

someFunc :: IO ()
someFunc = do
  let query =
        SplatoonStageScheduleQuery
          { language = "ja",
            filters =
              [ MatchTypeFilter
                  { matchType = "regular",
                    stages = Just $ StageFilter {matchBothStages = False, stageIds = [1, 2]},
                    rules = Just ["TURF_WAR"],
                    timeSlots = Just [TimeSlot {start = "00:00", end = "01:00", utcOffset = Just "+09:00", dayOfWeek = Just "Monday"}],
                    notifications = Just [NotificationSetting {minutesBefore = 15}]
                  }
              ]
          }
  events <- generateICalEvents query
  P.putStrLn $ ICal.buildICalText events (language query)
