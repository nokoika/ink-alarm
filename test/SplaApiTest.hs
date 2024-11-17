module SplaApiTest (test) where

import qualified Data.Aeson as A (eitherDecode)
import qualified Data.ByteString.Lazy as BL (readFile)
import qualified SplaApi
  ( DefaultSchedule (..),
    EventMatch (..),
    EventSummary (..),
    Result (..),
    Root (..),
    Rule (..),
    Stage (..),
  )
import Test.Hspec (describe, hspec, it, shouldBe)
import Prelude (Bool (..), Either (..), IO, Maybe (..), String, ($))

test :: IO ()
test = hspec $ do
  describe "JSON Parser" $ do
    it "should parse JSON" $ do
      jsonString <- BL.readFile "test/resources/splaapi.json"
      let actual = A.eitherDecode jsonString :: Either String SplaApi.Root
      let expect =
            Right
              ( SplaApi.Root
                  { result =
                      SplaApi.Result
                        { regular =
                            [ SplaApi.DefaultSchedule
                                { startTime = "2024-11-17T21:00:00+09:00",
                                  endTime = "2024-11-17T23:00:00+09:00",
                                  rule = Just (SplaApi.Rule {key = "TURF_WAR", name = "ナワバリバトル"}),
                                  stages =
                                    Just
                                      [ SplaApi.Stage
                                          { id = 22,
                                            name = "ネギトロ炭鉱",
                                            image = "https://example.com"
                                          }
                                      ],
                                  isFest = False
                                }
                            ],
                          bankaraOpen =
                            [ SplaApi.DefaultSchedule
                                { startTime = "2024-11-17T21:00:00+09:00",
                                  endTime = "2024-11-17T23:00:00+09:00",
                                  rule = Just (SplaApi.Rule {key = "AREA", name = "ガチエリア"}),
                                  stages =
                                    Just
                                      [ SplaApi.Stage
                                          { id = 9,
                                            name = "ヒラメが丘団地",
                                            image = "https://example.com"
                                          }
                                      ],
                                  isFest = False
                                }
                            ],
                          bankaraChallenge =
                            [ SplaApi.DefaultSchedule
                                { startTime = "2024-11-17T21:00:00+09:00",
                                  endTime = "2024-11-17T23:00:00+09:00",
                                  rule = Just (SplaApi.Rule {key = "GOAL", name = "ガチホコバトル"}),
                                  stages =
                                    Just
                                      [ SplaApi.Stage
                                          { id = 10,
                                            name = "マサバ海峡大橋",
                                            image = "https://example.com"
                                          }
                                      ],
                                  isFest = False
                                }
                            ],
                          event =
                            [ SplaApi.EventMatch
                                { startTime = "2024-11-20T11:00:00+09:00",
                                  endTime = "2024-11-20T13:00:00+09:00",
                                  rule = (SplaApi.Rule {key = "AREA", name = "ガチエリア"}),
                                  stages =
                                    [ SplaApi.Stage
                                        { id = 10,
                                          name = "マサバ海峡大橋",
                                          image = "https://example.com"
                                        }
                                    ],
                                  event =
                                    SplaApi.EventSummary
                                      { id = "FastMove",
                                        name = "イカダッシュバトル",
                                        desc = "イカダッシュ速度アップ！ イカやタコでステージを泳ぎ回れ！"
                                      },
                                  isFest = False
                                }
                            ],
                          x =
                            [ SplaApi.DefaultSchedule
                                { startTime = "2024-11-17T21:00:00+09:00",
                                  endTime = "2024-11-17T23:00:00+09:00",
                                  rule = Just (SplaApi.Rule {key = "CLAM", name = "ガチアサリ"}),
                                  stages =
                                    Just
                                      [ SplaApi.Stage
                                          { id = 23,
                                            name = "カジキ空港",
                                            image = "https://example.com"
                                          }
                                      ],
                                  isFest = False
                                }
                            ]
                        }
                  }
              )

      actual `shouldBe` expect
