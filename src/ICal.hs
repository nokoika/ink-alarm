module ICal (ICalEvent (..), Reminder (..), ReminderTrigger (..), ReminderAction (..), buildICalText) where

import qualified Data.List as List (intercalate)
import qualified Data.Maybe as Maybe (fromMaybe)
import Prelude (Enum, Eq, Int, Maybe, Show, String, (++), (.))
import qualified Prelude as P (Show (show), concatMap)

-- ICalEventデータ型
data ICalEvent = ICalEvent
  { summary :: String,
    description :: String,
    start :: String, -- ISO 8601
    end :: String, -- ISO 8601
    url :: Maybe String,
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

buildICalText :: [ICalEvent] -> String -> String
buildICalText events language =
  List.intercalate
    "\n"
    [ "BEGIN:VCALENDAR",
      "VERSION:2.0",
      "PRODID:-//Splatoon 3//" ++ language,
      aggregate
        ( \ICalEvent {summary, description, start, end, url, reminders} ->
            [ "BEGIN:VEVENT",
              "SUMMARY:" ++ summary,
              "DESCRIPTION:" ++ description,
              "DTSTART:" ++ start,
              "DTEND:" ++ end,
              "URL:" ++ Maybe.fromMaybe "" url,
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
