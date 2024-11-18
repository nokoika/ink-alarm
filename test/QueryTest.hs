module QueryTest (test) where

import qualified Data.Text.IO as T (readFile)
import qualified Query
  ( FilterCondition (..),
    NotificationSetting (..),
    QueryRoot (..),
    StageFilter (..),
    TimeSlot (..),
    parseBase64Url,
  )
import Test.Hspec (describe, hspec, it, shouldBe)
import Prelude (Bool (..), Either (..), IO, Maybe (..), ($))

test :: IO ()
test = hspec $ do
  describe "Query Parser" $ do
    it "hoge" $ do
      base64Url <- T.readFile "test/resources/query-base64url.txt"
      let actual = Query.parseBase64Url base64Url
      let expect =
            Right
              ( Query.QueryRoot
                  { Query.language = "ja",
                    Query.utcOffset = "+09:00",
                    Query.filters =
                      [ Query.FilterCondition
                          { Query.matchType = "bankara_open",
                            Query.stages =
                              Just
                                ( Query.StageFilter
                                    { Query.matchBothStages = True,
                                      Query.stageIds = [22]
                                    }
                                ),
                            Query.rules = Just ["TURF_WAR"],
                            Query.timeSlots =
                              Just
                                [ Query.TimeSlot
                                    { Query.start = "00:00",
                                      Query.end = "06:00",
                                      Query.dayOfWeek = Just "sun"
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
