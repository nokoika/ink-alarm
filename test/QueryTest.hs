module QueryTest (test) where

import qualified Data.Text.IO as T (readFile)
import qualified Query
  ( DayOfWeek (..),
    Language (..),
    FilterCondition (..),
    NotificationSetting (..),
    QueryRoot (..),
    StageFilter (..),
    TimeSlot (..),
    TimeSlotTimeOfDay (..),
    MatchType (..),
    parseBase64Url,
    Rule (..),
  )
import Test.Hspec (describe, hspec, it, shouldBe)
import Prelude (Bool (..), Either (..), IO, Maybe (..), ($))
import qualified Data.Time.LocalTime as LT (TimeOfDay (..))

test :: IO ()
test = hspec $ do
  describe "Query Parser" $ do
    it "hoge" $ do
      base64Url <- T.readFile "test/resources/query-base64url.txt"
      let actual = Query.parseBase64Url base64Url
      let expect =
            Right
              ( Query.QueryRoot
                  { Query.language = Query.Japanese,
                    Query.utcOffset = "+09:00",
                    Query.filters =
                      [ Query.FilterCondition
                          { Query.matchType = Query.BankaraOpen,
                            Query.stages =
                              Just
                                ( Query.StageFilter
                                    { Query.matchBothStages = True,
                                      Query.stageIds = [22]
                                    }
                                ),
                            Query.rules = Just [Query.TurfWar],
                            Query.timeSlots =
                              Just
                                [ Query.TimeSlot
                                    { Query.start = Query.TimeSlotTimeOfDay { timeOfDay = LT.TimeOfDay 0 0 0 },
                                      Query.end = Query.TimeSlotTimeOfDay { timeOfDay = LT.TimeOfDay 6 0 0 },
                                      Query.dayOfWeek = Just Query.Sunday
                                    }
                                ],
                            Query.notifications =
                              Just
                                [ Query.NotificationSetting
                                    { Query.minutesBefore = 90
                                    }
                                ]
                          }
                      ]
                  }
              )
      actual `shouldBe` expect
