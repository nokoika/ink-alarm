module Filter.Internal.ICal
  ( createICalInput,
    createICalEventsFromDefaultSchedules,
    createICalEventsFromEventMatches,
  )
where

import Data.Function (on)
import qualified Data.List as L
import qualified Date
import qualified Filter.Internal.Schedule as FS
import qualified Hash
import qualified ICal as I
import qualified Query as Q
import qualified SplaApi as S
import qualified Translation
import Prelude (Maybe (Just), String, show, ($), (++), (==))

eventId :: Q.Language -> Q.Mode -> S.Rule -> [S.Stage] -> Date.UTCTimeRange -> String
eventId language mode apiRule apiStages utcTimeRange =
  Hash.sha256Hash $ show utcTimeRange ++ show apiRule ++ show apiStages ++ show language ++ show mode

uniqueById :: [I.ICalEvent] -> [I.ICalEvent]
uniqueById = L.nubBy ((==) `on` I.id)

createICalEventsFromDefaultSchedules :: String -> Q.QueryRoot -> [S.DefaultSchedule] -> Q.Mode -> [I.ICalEvent]
createICalEventsFromDefaultSchedules settingsUrl Q.QueryRoot {utcOffset, filters, language} defaultSchedules mode =
  uniqueById
    [ I.ICalEvent
        { I.id = eventId language mode apiRule apiStages (startTime, endTime),
          I.summary = Translation.showCalendarSummary language mode apiRule apiStages,
          I.description = Translation.showCalendarDescription language mode apiRule apiStages timeRange settingsUrl,
          I.start = intersectStart,
          I.end = intersectEnd
        }
      | defaultSchedule@S.DefaultSchedule {startTime, endTime, rule = Just apiRule, stages = Just apiStages} <- defaultSchedules,
        let Q.UtcOffsetTimeZone utcOffset' = utcOffset,
        let timeRange = Date.convertRangedUTCTimeToZonedTime utcOffset' (startTime, endTime),
        filter <- filters,
        (intersectStart, intersectEnd) <- FS.getMatchedTimeRangesFromDefaultSchedule filter defaultSchedule utcOffset' mode
    ]

createICalEventsFromEventMatches :: String -> Q.QueryRoot -> [S.EventMatch] -> [I.ICalEvent]
createICalEventsFromEventMatches settingsUrl Q.QueryRoot {utcOffset, filters, language} eventMatches =
  [ I.ICalEvent
      { -- API では日本語のイベント名しか手に入らないので、日本語以外の場合は末尾にイベント名を追加する。descも同様
        I.id = eventId language Q.Event apiRule apiStages (startTime, endTime),
        I.summary =
          if language == Q.Japanese
            then eventName ++ baseSummary
            else baseSummary ++ " / " ++ eventName,
        I.description =
          if language == Q.Japanese
            then eventDescription ++ "\n\n" ++ baseDescription
            else baseDescription ++ "\n\n" ++ eventDescription,
        I.start = intersectStart,
        I.end = intersectEnd
      }
    | eventMatch@S.EventMatch
        { S.startTime,
          S.endTime,
          S.rule = apiRule,
          S.stages = apiStages,
          eventSummary = S.EventSummary {name = eventName, desc = eventDescription}
        } <-
        eventMatches,
      let Q.UtcOffsetTimeZone utcOffset' = utcOffset,
      let timeRange = Date.convertRangedUTCTimeToZonedTime utcOffset' (startTime, endTime),
      let baseSummary = Translation.showCalendarSummary language Q.Event apiRule apiStages,
      let baseDescription = Translation.showCalendarDescription language Q.Event apiRule apiStages timeRange settingsUrl,
      filter <- filters,
      (intersectStart, intersectEnd) <- FS.getMatchedTimeRangesFromEventMatch filter eventMatch utcOffset'
  ]

createICalInput :: String -> Q.QueryRoot -> S.Result -> I.ICalInput
createICalInput settingsUrl queryRoot result =
  I.ICalInput
    { I.language = Q.language queryRoot,
      I.settingsUrl = settingsUrl,
      I.events = regular ++ bankaraChallenge ++ bankaraOpen ++ x ++ event
    }
  where
    regular = createICalEventsFromDefaultSchedules settingsUrl queryRoot (S.regular result) Q.Regular
    bankaraChallenge = createICalEventsFromDefaultSchedules settingsUrl queryRoot (S.bankaraChallenge result) Q.BankaraChallenge
    bankaraOpen = createICalEventsFromDefaultSchedules settingsUrl queryRoot (S.bankaraOpen result) Q.BankaraOpen
    x = createICalEventsFromDefaultSchedules settingsUrl queryRoot (S.x result) Q.XMatch
    event = createICalEventsFromEventMatches settingsUrl queryRoot (S.event result)
