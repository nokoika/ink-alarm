module Test.Query (test) where

import qualified Data.Text.IO as TIO (readFile)
import qualified Data.Time.Calendar as C
import qualified Data.Time.LocalTime as LT
import qualified Query
  ( FilterCondition (..),
    Language (..),
    Mode (..),
    QueryRoot (..),
    Rule (..),
    StageFilter (..),
    TimeSlot (..),
    TimeSlotDayOfWeek (..),
    TimeSlotTimeOfDay (..),
    UtcOffsetTimeZone (..),
    parseBase64Url,
  )
import Test.Hspec (describe, hspec, it, shouldBe)
import qualified TestUtil as TU
import Prelude (Bool (True), Either (Right), IO, Maybe (Just), ($))

test :: IO ()
test = hspec $ do
  describe "Query Parser" $ do
    it "can parse base64Url json" $ do
      base64Url <- TIO.readFile "test/resources/query-base64url.txt"
      let actual = Query.parseBase64Url base64Url
      let expect =
            Right
              ( Query.QueryRoot
                  { Query.language = Query.Japanese,
                    Query.utcOffset = Query.UtcOffsetTimeZone $ TU.createTimeZone 9 "",
                    Query.filters =
                      [ Query.FilterCondition
                          { Query.modes = Just [Query.BankaraOpen],
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
                                    { Query.start = Query.TimeSlotTimeOfDay $ LT.TimeOfDay 0 0 0,
                                      Query.end = Query.TimeSlotTimeOfDay $ LT.TimeOfDay 6 0 0,
                                      Query.dayOfWeeks = Just [Query.TimeSlotDayOfWeek C.Sunday]
                                    }
                                ]
                          }
                      ]
                  }
              )
      actual `shouldBe` expect
