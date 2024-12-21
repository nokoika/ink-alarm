module ICal (ICalInput (..), ICalEvent (..), Reminder (..), ReminderTrigger (..), ReminderAction (..), buildICalText) where

import qualified Data.Char as C
import Data.List (intercalate)
import qualified Data.Time as T
import qualified Query as Q
import Prelude (Enum, Eq, Int, Show (show), String, concatMap, filter, (++), (.))

data ICalInput = ICalInput
  { language :: Q.Language,
    events :: [ICalEvent]
  }
  deriving (Show, Eq)

data ICalEvent = ICalEvent
  { summary :: String,
    description :: String,
    start :: T.UTCTime,
    end :: T.UTCTime,
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
  show ReminderTrigger {time} = "-PT" ++ show time ++ "M"

data Reminder = Reminder
  { trigger :: ReminderTrigger,
    action :: ReminderAction
  }
  deriving (Show, Eq)

convertUTCTimeToLocalTime :: T.UTCTime -> T.LocalTime
convertUTCTimeToLocalTime = T.utcToLocalTime T.utc

showLanguage :: Q.Language -> String
showLanguage Q.Japanese = "JA"
showLanguage Q.English = "EN"

-- 20231207T120000Z <- 2023-12-07T12:00:00Z
toICalTime :: T.UTCTime -> String
toICalTime utcTime = day ++ "T" ++ dayTime ++ "Z"
  where
    T.LocalTime {localDay, localTimeOfDay} = convertUTCTimeToLocalTime utcTime
    convert s = filter C.isDigit (show s)
    day = convert localDay
    dayTime = convert localTimeOfDay

buildICalText :: ICalInput -> String
buildICalText ICalInput {language, events} =
  intercalate
    "\n"
    [ "BEGIN:VCALENDAR",
      "VERSION:2.0",
      "PRODID:-//Splatoon 3//" ++ showLanguage language,
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
                      "TRIGGER:" ++ show trigger,
                      "ACTION:" ++ show action,
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
    aggregate f = intercalate "\n" . concatMap f
