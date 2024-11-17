module QueryTest (test) where

import Test.Hspec (hspec, describe, it, shouldBe)
import qualified Query
  ( FilterCondition (..),
    NotificationSetting (..),
    QueryRoot (..),
    StageFilter (..),
    TimeSlot (..),
    parseBase64Url
  )
import Prelude (Bool (..), Either (..), IO, Maybe (..), String, ($))

test :: IO ()
test = hspec $ do
  describe "Query Parser" $ do
    it "hoge" $ do
      let actual = Query.parseBase64Url ""
      let expect =
            Right
              ( Query.QueryRoot
                  { Query.language = "ja",
                    Query.filters =
                      [ Query.FilterCondition
                          { Query.matchType = "bankara_open",
                            Query.stages =
                              Just
                                ( Query.StageFilter
                                    { Query.matchBothStages = False,
                                      Query.stageIds = [22]
                                    }
                                ),
                            Query.rules = Just ["TURF_WAR"],
                            Query.timeSlots =
                              Just
                                [ Query.TimeSlot
                                    { Query.start = "20:00",
                                      Query.end = "20:00",
                                      Query.utcOffset = Just "+09:00",
                                      Query.dayOfWeek = Just "Monday"
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
