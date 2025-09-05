module Test.Service.ICal (test) where

import App.Config (Config (..))
import App.Context (AppContext (..))
import App.Monad (runAppM)
import qualified Query
import qualified Service.ICal
import qualified SplaApi
  ( DefaultSchedule (..),
    Result (..),
    Root (..),
    Rule (..),
    RuleKey (..),
    Stage (..),
  )
import qualified SplaApi.Cached as Cached
import Test.Hspec
import qualified TestUtil as TU
import Prelude

test :: IO ()
test = hspec $ do
  describe "Service.ICal" $ do
    describe "generateICalText" $ do
      it "should generate valid iCal text for regular battle query" $ do
        -- Setup
        cache <- Cached.initScheduleCache
        let ctx =
              AppContext
                { acScheduleCache = cache,
                  acConfig = Config {configPort = 8080}
                }

        -- Create test query
        let query =
              Query.QueryRoot
                { Query.language = Query.Japanese,
                  Query.utcOffset = Query.UtcOffsetTimeZone $ TU.createTimeZone 9 "",
                  Query.filters =
                    [ Query.FilterCondition
                        { Query.modes = Just [Query.Regular],
                          Query.stages = Nothing,
                          Query.rules = Nothing,
                          Query.timeSlots = Nothing
                        }
                    ]
                }

        -- Create minimal API response
        let apiData = createTestApiData

        -- Execute
        result <- runAppM ctx (Service.ICal.generateICalText query apiData)

        -- Verify
        case result of
          Left err -> expectationFailure $ "Unexpected error: " ++ show err
          Right icalText -> do
            icalText `shouldContain` "BEGIN:VCALENDAR"
            icalText `shouldContain` "VERSION:2.0"
            icalText `shouldContain` "END:VCALENDAR"

      it "should handle empty schedule data gracefully" $ do
        -- Setup
        cache <- Cached.initScheduleCache
        let ctx =
              AppContext
                { acScheduleCache = cache,
                  acConfig = Config {configPort = 8080}
                }

        let query =
              Query.QueryRoot
                { Query.language = Query.Japanese,
                  Query.utcOffset = Query.UtcOffsetTimeZone $ TU.createTimeZone 9 "",
                  Query.filters = []
                }

        let emptyApiData = createEmptyApiData

        -- Execute
        result <- runAppM ctx (Service.ICal.generateICalText query emptyApiData)

        -- Verify
        case result of
          Left err -> expectationFailure $ "Unexpected error: " ++ show err
          Right icalText -> do
            icalText `shouldContain` "BEGIN:VCALENDAR"
            icalText `shouldContain` "VERSION:2.0"
            icalText `shouldContain` "END:VCALENDAR"

      it "should handle multiple filter conditions" $ do
        -- Setup
        cache <- Cached.initScheduleCache
        let ctx =
              AppContext
                { acScheduleCache = cache,
                  acConfig = Config {configPort = 8080}
                }

        let query =
              Query.QueryRoot
                { Query.language = Query.English,
                  Query.utcOffset = Query.UtcOffsetTimeZone $ TU.createTimeZone (-7) "",
                  Query.filters =
                    [ Query.FilterCondition
                        { Query.modes = Just [Query.Regular],
                          Query.stages = Nothing,
                          Query.rules = Just [Query.TowerControl, Query.SplatZones],
                          Query.timeSlots = Nothing
                        },
                      Query.FilterCondition
                        { Query.modes = Just [Query.BankaraOpen],
                          Query.stages = Nothing,
                          Query.rules = Nothing,
                          Query.timeSlots = Nothing
                        }
                    ]
                }

        let apiData = createTestApiData

        -- Execute
        result <- runAppM ctx (Service.ICal.generateICalText query apiData)

        -- Verify
        case result of
          Left err -> expectationFailure $ "Unexpected error: " ++ show err
          Right icalText -> do
            icalText `shouldContain` "BEGIN:VCALENDAR"
            icalText `shouldSatisfy` (not . null)

      it "should respect timezone settings in output" $ do
        -- Setup
        cache <- Cached.initScheduleCache
        let ctx =
              AppContext
                { acScheduleCache = cache,
                  acConfig = Config {configPort = 8080}
                }

        -- Test with UTC timezone
        let queryUTC =
              Query.QueryRoot
                { Query.language = Query.English,
                  Query.utcOffset = Query.UtcOffsetTimeZone $ TU.createTimeZone 0 "",
                  Query.filters =
                    [ Query.FilterCondition
                        { Query.modes = Just [Query.Regular],
                          Query.stages = Nothing,
                          Query.rules = Nothing,
                          Query.timeSlots = Nothing
                        }
                    ]
                }

        let apiData = createTestApiDataWithSchedule

        -- Execute
        result <- runAppM ctx (Service.ICal.generateICalText queryUTC apiData)

        -- Verify
        case result of
          Left err -> expectationFailure $ "Unexpected error: " ++ show err
          Right icalText -> do
            -- iCal should contain timezone-aware timestamps
            icalText `shouldContain` "BEGIN:VCALENDAR"
            icalText `shouldContain` "DTSTART"
            icalText `shouldContain` "DTEND"

-- Helper functions to create test data
createTestApiData :: SplaApi.Root
createTestApiData =
  SplaApi.Root
    { SplaApi.result =
        SplaApi.Result
          { SplaApi.regular = [],
            SplaApi.bankaraOpen = [],
            SplaApi.bankaraChallenge = [],
            SplaApi.x = [],
            SplaApi.event = []
          }
    }

createEmptyApiData :: SplaApi.Root
createEmptyApiData = createTestApiData

createTestApiDataWithSchedule :: SplaApi.Root
createTestApiDataWithSchedule =
  SplaApi.Root
    { SplaApi.result =
        SplaApi.Result
          { SplaApi.regular =
              [ SplaApi.DefaultSchedule
                  { SplaApi.startTime = TU.createUTCTime 2025 1 5 10 0,
                    SplaApi.endTime = TU.createUTCTime 2025 1 5 12 0,
                    SplaApi.rule = Just (SplaApi.Rule {SplaApi.key = SplaApi.TurfWar, SplaApi.name = "Turf War"}),
                    SplaApi.stages =
                      Just
                        [ SplaApi.Stage
                            { SplaApi.id = 1,
                              SplaApi.name = "Test Stage 1",
                              SplaApi.image = "http://example.com/stage1.png"
                            },
                          SplaApi.Stage
                            { SplaApi.id = 2,
                              SplaApi.name = "Test Stage 2",
                              SplaApi.image = "http://example.com/stage2.png"
                            }
                        ],
                    SplaApi.isFest = False
                  }
              ],
            SplaApi.bankaraOpen = [],
            SplaApi.bankaraChallenge = [],
            SplaApi.x = [],
            SplaApi.event = []
          }
    }
