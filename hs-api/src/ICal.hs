module ICal (ICalInput (..), ICalEvent (..), Reminder (..), ReminderTrigger (..), ReminderAction (..), buildICalText) where

import qualified Data.Char as C
import Data.List (intercalate)
import qualified Data.Time as T
import qualified Hash
import qualified Query as Q
import qualified Translation
import Prelude (Enum, Eq, Int, Show (show), String, concatMap, filter, ($), (++), (.), (==))

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
      "PRODID:" ++ Translation.showICalProdId language,
      "METHOD:PUBLISH",
      "CALSCALE:GREGORIAN",
      "X-WR-CALNAME:" ++ Translation.showApplicationName language,
      -- ※本当はDTSTAMPもrequiredだが、セットすべき値は慎重に検討すべきであるため暫定的に省略
      aggregate
        ( \icalEvent@ICalEvent {summary, description, start, end, reminders} ->
            [ "BEGIN:VEVENT",
              "UID:" ++ eventId icalEvent,
              "SUMMARY:" ++ summary,
              "DESCRIPTION:" ++ escapeNewLine description,
              "DTSTART:" ++ toICalTime start,
              "DTEND:" ++ toICalTime end,
              aggregate
                ( \Reminder {trigger, action} ->
                    [ "BEGIN:VALARM",
                      "TRIGGER:" ++ show trigger,
                      "ACTION:" ++ show action,
                      "DESCRIPTION:" ++ summary, -- ※通知文言を凝ってもよい気がするが、今後VALARM自体を消すかもしれないので暫定的にsummaryを設定
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
    escapeNewLine :: String -> String
    escapeNewLine = concatMap (\c -> if c == '\n' then "\\n" else [c])
    eventId :: ICalEvent -> String
    eventId ICalEvent {summary, start, end, reminders} =
      Hash.sha256Hash $
        show start ++ show end ++ summary ++ concatMap show reminders
