module ICal (ICalInput (..), ICalEvent (..), Reminder (..), ReminderTrigger (..), ReminderAction (..), buildICalText) where

import qualified Data.List as List (intercalate)
import Prelude (Enum, Eq, Int, Show, String, (++), (.))
import qualified Prelude as P (Show (show), concatMap, filter)
import Data.Time (UTCTime (..), LocalTime(..), utcToLocalTime, utc)
import qualified Data.Char as C (isDigit)

data ICalInput = ICalInput
  { language :: String, -- "JA" | "EN"
    events :: [ICalEvent]
  }
  deriving (Show, Eq)

data ICalEvent = ICalEvent
  { summary :: String,
    description :: String,
    start :: UTCTime, -- ISO 8601
    end :: UTCTime, -- ISO 8601
    reminders :: [Reminder]
  }
  deriving (Show, Eq)

data ReminderAction = Display | Email deriving (Eq, Enum)

instance Show ReminderAction where
  show Display = "DISPLAY"
  show Email = "EMAIL"

-- TODO: 絶対時刻も指定できるようにする
newtype ReminderTrigger = ReminderTrigger
  { time :: Int
  }
  deriving (Eq)

instance Show ReminderTrigger where
  show ReminderTrigger {time} = "-PT" ++ P.show time ++ "M"

data Reminder = Reminder
  { trigger :: ReminderTrigger,
    action :: ReminderAction
  }
  deriving (Show, Eq)

convertUTCTimeToLocalTime :: UTCTime -> LocalTime
convertUTCTimeToLocalTime = utcToLocalTime utc

-- 20231207T120000Z <- 2023-12-07T12:00:00Z
toICalTime :: UTCTime -> String
toICalTime utcTime = day ++ "T" ++ dayTime ++ "Z"
  where
    LocalTime { localDay, localTimeOfDay } = convertUTCTimeToLocalTime utcTime
    convert s = P.filter C.isDigit (P.show s)
    day = convert localDay
    dayTime = convert localTimeOfDay


buildICalText :: ICalInput -> String
buildICalText ICalInput {language, events} =
  List.intercalate
    "\n"
    [ "BEGIN:VCALENDAR",
      "VERSION:2.0",
      "PRODID:-//Splatoon 3//" ++ language,
      aggregate
        ( \ICalEvent {summary, description, start, end, reminders} ->
            [ "BEGIN:VEVENT",
              "SUMMARY:" ++ summary,
              "DESCRIPTION:" ++ description,
              "DTSTART:" ++ toICalTime start,
              "DTEND:" ++ toICalTime end,
              aggregate
                ( \Reminder {trigger, action} ->
                    [ "BEGIN:VALARM",
                      "TRIGGER:" ++ P.show trigger,
                      "ACTION:" ++ P.show action,
                      "END:VALARM"
                    ]
                )
                reminders,
              "END:VEVENT"
            ]
        )
        events,
      "END:VCALENDAR"
    ]
  where
    aggregate :: (a -> [String]) -> [a] -> String
    aggregate f = List.intercalate "\n" . P.concatMap f
