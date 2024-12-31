module Test.Filter.Internal.ICal (test) where

import qualified Data.Time as T
import qualified Filter.Internal.ICal as FI
import qualified ICal as I
import qualified Query as Q
import qualified SplaApi as S
import Test.Hspec (describe, hspec, it, shouldBe)
import qualified TestUtil as TU
import Prelude (Bool (False), IO, Maybe (Just), ($))

test :: IO ()
test = hspec $ do
  describe "createICalInput" $ do
    it "正常系　日本人向け" $ do
      let splaApiResult =
            S.Result
              { regular =
                  [ S.DefaultSchedule
                      { startTime = TU.createUTCTime 2024 11 17 12 0,
                        endTime = TU.createUTCTime 2024 11 17 14 0,
                        rule = Just (S.Rule {key = S.TurfWar, name = "ナワバリバトル"}),
                        stages =
                          Just
                            [ S.Stage
                                { id = 22,
                                  name = "ネギトロ炭鉱",
                                  image = "https://example.com"
                                }
                            ],
                        isFest = False
                      }
                  ],
                bankaraOpen =
                  [ S.DefaultSchedule
                      { startTime = TU.createUTCTime 2024 11 17 12 0,
                        endTime = TU.createUTCTime 2024 11 17 14 0,
                        rule = Just (S.Rule {key = S.SplatZones, name = "ガチエリア"}),
                        stages =
                          Just
                            [ S.Stage
                                { id = 9,
                                  name = "ヒラメが丘団地",
                                  image = "https://example.com"
                                }
                            ],
                        isFest = False
                      }
                  ],
                bankaraChallenge =
                  [ S.DefaultSchedule
                      { startTime = TU.createUTCTime 2024 11 17 12 0, -- 日本時間 21:00 から 23:00, 日曜日
                        endTime = TU.createUTCTime 2024 11 17 14 0,
                        rule = Just (S.Rule {key = S.Rainmaker, name = "ガチホコバトル"}),
                        stages =
                          Just
                            [ S.Stage
                                { id = 10,
                                  name = "マサバ海峡大橋",
                                  image = "https://example.com"
                                },
                              S.Stage
                                { id = 9,
                                  name = "ヒラメが丘団地",
                                  image = "https://example.com"
                                }
                            ],
                        isFest = False
                      }
                  ],
                event =
                  [ S.EventMatch
                      { startTime = TU.createUTCTime 2024 11 20 2 0,
                        endTime = TU.createUTCTime 2024 11 20 4 0,
                        rule = (S.Rule {key = S.SplatZones, name = "ガチエリア"}),
                        stages =
                          [ S.Stage
                              { id = 10,
                                name = "マサバ海峡大橋",
                                image = "https://example.com"
                              }
                          ],
                        eventSummary =
                          S.EventSummary
                            { id = "FastMove",
                              name = "イカダッシュバトル",
                              desc = "イカダッシュ速度アップ！ イカやタコでステージを泳ぎ回れ！"
                            },
                        isFest = False
                      }
                  ],
                x =
                  [ S.DefaultSchedule
                      { startTime = TU.createUTCTime 2024 11 17 12 0,
                        endTime = TU.createUTCTime 2024 11 17 14 0,
                        rule = Just (S.Rule {key = S.ClamBlitz, name = "ガチアサリ"}),
                        stages =
                          Just
                            [ S.Stage
                                { id = 23,
                                  name = "カジキ空港",
                                  image = "https://example.com"
                                }
                            ],
                        isFest = False
                      }
                  ]
              }

      let queryRoot =
            Q.QueryRoot
              { Q.language = Q.Japanese,
                Q.utcOffset = Q.UtcOffsetTimeZone $ TU.createTimeZone 9 "",
                Q.filters =
                  [ Q.FilterCondition
                      { Q.modes = Just [Q.BankaraChallenge],
                        Q.timeSlots =
                          Just
                            [ Q.TimeSlot
                                { Q.start = Q.TimeSlotTimeOfDay $ T.TimeOfDay 20 0 0,
                                  Q.end = Q.TimeSlotTimeOfDay $ T.TimeOfDay 0 0 0,
                                  Q.dayOfWeek = Just $ Q.TimeSlotDayOfWeek T.Sunday
                                }
                            ],
                        Q.stages =
                          Just
                            Q.StageFilter
                              { Q.matchBothStages = False,
                                Q.stageIds = [10, 4]
                              },
                        Q.rules = Just [Q.Rainmaker]
                      }
                  ]
              }

      let icalInput = FI.createICalInput queryRoot splaApiResult
      icalInput
        `shouldBe` I.ICalInput
          { I.language = Q.Japanese,
            I.events =
              [ I.ICalEvent
                  { I.id = "9864650fe1bd4d9a8fba92577fd4b7cd395cdeeddf78f052f1cfec2fb65df705",
                    I.summary = "【ガチホコバトル】バンカラチャレンジ / マサバ海峡大橋, ヒラメが丘団地",
                    I.description = "21:00から23:00までガチホコバトルの予定があります。\n・バンカラチャレンジ\n・ステージ: マサバ海峡大橋, ヒラメが丘団地",
                    I.start = TU.createUTCTime 2024 11 17 12 0,
                    I.end = TU.createUTCTime 2024 11 17 14 0
                  }
              ]
          }

    it "正常系　北米向け(ロサンゼルス想定)" $ do
      let splaApiResult =
            S.Result
              { regular =
                  [ S.DefaultSchedule
                      { startTime = TU.createUTCTime 2024 11 17 12 0,
                        endTime = TU.createUTCTime 2024 11 17 14 0,
                        rule = Just (S.Rule {key = S.TurfWar, name = "ナワバリバトル"}),
                        stages =
                          Just
                            [ S.Stage
                                { id = 22,
                                  name = "ネギトロ炭鉱",
                                  image = "https://example.com"
                                }
                            ],
                        isFest = False
                      }
                  ],
                bankaraOpen =
                  [ S.DefaultSchedule
                      { startTime = TU.createUTCTime 2024 11 17 12 0,
                        endTime = TU.createUTCTime 2024 11 17 14 0,
                        rule = Just (S.Rule {key = S.SplatZones, name = "ガチエリア"}),
                        stages =
                          Just
                            [ S.Stage
                                { id = 9,
                                  name = "ヒラメが丘団地",
                                  image = "https://example.com"
                                }
                            ],
                        isFest = False
                      }
                  ],
                bankaraChallenge =
                  [ S.DefaultSchedule
                      { startTime = TU.createUTCTime 2024 11 17 12 0, -- ロサンゼルス時間 04:00 から 06:00, 日曜日
                        endTime = TU.createUTCTime 2024 11 17 14 0,
                        rule = Just (S.Rule {key = S.Rainmaker, name = "ガチホコバトル"}),
                        stages =
                          Just
                            [ S.Stage
                                { id = 10,
                                  name = "マサバ海峡大橋",
                                  image = "https://example.com"
                                },
                              S.Stage
                                { id = 9,
                                  name = "ヒラメが丘団地",
                                  image = "https://example.com"
                                }
                            ],
                        isFest = False
                      }
                  ],
                event =
                  [ S.EventMatch
                      { startTime = TU.createUTCTime 2024 11 20 2 0,
                        endTime = TU.createUTCTime 2024 11 20 4 0,
                        rule = (S.Rule {key = S.SplatZones, name = "ガチエリア"}),
                        stages =
                          [ S.Stage
                              { id = 10,
                                name = "マサバ海峡大橋",
                                image = "https://example.com"
                              }
                          ],
                        eventSummary =
                          S.EventSummary
                            { id = "FastMove",
                              name = "イカダッシュバトル",
                              desc = "イカダッシュ速度アップ！ イカやタコでステージを泳ぎ回れ！"
                            },
                        isFest = False
                      }
                  ],
                x =
                  [ S.DefaultSchedule
                      { startTime = TU.createUTCTime 2024 11 17 12 0,
                        endTime = TU.createUTCTime 2024 11 17 14 0,
                        rule = Just (S.Rule {key = S.ClamBlitz, name = "ガチアサリ"}),
                        stages =
                          Just
                            [ S.Stage
                                { id = 23,
                                  name = "カジキ空港",
                                  image = "https://example.com"
                                }
                            ],
                        isFest = False
                      }
                  ]
              }
      let queryRoot =
            Q.QueryRoot
              { Q.language = Q.English,
                Q.utcOffset = Q.UtcOffsetTimeZone $ TU.createTimeZone (-8) "", -- ロサンゼルスは UTC-8
                Q.filters =
                  [ Q.FilterCondition
                      { Q.modes = Just [Q.BankaraChallenge],
                        Q.timeSlots =
                          Just
                            [ Q.TimeSlot
                                { Q.start = Q.TimeSlotTimeOfDay $ T.TimeOfDay 3 0 0,
                                  Q.end = Q.TimeSlotTimeOfDay $ T.TimeOfDay 5 0 0,
                                  Q.dayOfWeek = Just $ Q.TimeSlotDayOfWeek T.Sunday
                                }
                            ],
                        Q.stages =
                          Just
                            Q.StageFilter
                              { Q.matchBothStages = False,
                                Q.stageIds = [10, 4]
                              },
                        Q.rules = Just [Q.Rainmaker]
                      }
                  ]
              }

      let icalInput = FI.createICalInput queryRoot splaApiResult
      icalInput
        `shouldBe` I.ICalInput
          { I.language = Q.English,
            I.events =
              [ I.ICalEvent
                  { I.id = "bb2afcd5dbc30d5027e6b196459d8eddc0e99af5108e4f87456826b77c282884",
                    I.summary = "【Rainmaker】Anarchy Battle (Series) / Hammerhead Bridge, Flounder Heights",
                    I.description = "There is a scheduled Rainmaker from 04:00 to 06:00.\n- Anarchy Battle (Series)\n- Stages: Hammerhead Bridge, Flounder Heights",
                    I.start = TU.createUTCTime 2024 11 17 12 0,
                    I.end = TU.createUTCTime 2024 11 17 13 0
                  }
              ]
          }

  describe "createICalEventsFromDefaultSchedules" $ do
    it "正常系" $ do
      let queryRoot =
            Q.QueryRoot
              { Q.language = Q.Japanese,
                Q.utcOffset = Q.UtcOffsetTimeZone $ TU.createTimeZone 9 "",
                Q.filters =
                  [ Q.FilterCondition
                      { Q.modes = Just [Q.BankaraChallenge],
                        Q.timeSlots =
                          Just
                            [ Q.TimeSlot
                                { Q.start = Q.TimeSlotTimeOfDay $ T.TimeOfDay 12 0 0,
                                  Q.end = Q.TimeSlotTimeOfDay $ T.TimeOfDay 16 0 0,
                                  Q.dayOfWeek = Just $ Q.TimeSlotDayOfWeek T.Friday
                                }
                            ],
                        Q.stages =
                          Just
                            Q.StageFilter
                              { Q.matchBothStages = False,
                                Q.stageIds = [1, 4]
                              },
                        Q.rules = Just [Q.ClamBlitz]
                      }
                  ]
              }
      let defaultSchedules =
            [ -- マッチする
              S.DefaultSchedule
                { S.startTime = TU.createUTCTime 2021 1 1 4 0, -- 金曜日
                  S.endTime = TU.createUTCTime 2021 1 1 6 0,
                  S.rule = Just $ S.Rule {key = S.ClamBlitz, name = ""},
                  S.stages = Just [S.Stage 1 "" "", S.Stage 2 "" ""],
                  S.isFest = False
                },
              -- マッチしない
              S.DefaultSchedule
                { S.startTime = TU.createUTCTime 2021 1 2 4 0, -- 金曜日じゃない
                  S.endTime = TU.createUTCTime 2021 1 2 6 0,
                  S.rule = Just $ S.Rule {key = S.ClamBlitz, name = ""},
                  S.stages = Just [S.Stage 1 "" "", S.Stage 2 "" ""],
                  S.isFest = False
                }
            ]
      let icalEvents = FI.createICalEventsFromDefaultSchedules queryRoot defaultSchedules Q.BankaraChallenge
      icalEvents
        `shouldBe` [ I.ICalEvent
                       { I.id = "13bd55fa792ff7180be0f964ca83029891cac218359fafd756a864a8addabbda",
                         I.summary = "【ガチアサリ】バンカラチャレンジ / ユノハナ大渓谷, ゴンズイ地区",
                         I.description = "13:00から15:00までガチアサリの予定があります。\n・バンカラチャレンジ\n・ステージ: ユノハナ大渓谷, ゴンズイ地区",
                         I.start = TU.createUTCTime 2021 1 1 4 0,
                         I.end = TU.createUTCTime 2021 1 1 6 0
                       }
                   ]

  describe "createICalEventsFromEventMatches" $ do
    it "正常系 日本語" $ do
      let queryRoot =
            Q.QueryRoot
              { Q.language = Q.Japanese,
                Q.utcOffset = Q.UtcOffsetTimeZone $ TU.createTimeZone 9 "",
                Q.filters =
                  [ Q.FilterCondition
                      { Q.modes = Just [Q.Event],
                        Q.timeSlots =
                          Just
                            [ Q.TimeSlot
                                { Q.start = Q.TimeSlotTimeOfDay $ T.TimeOfDay 12 0 0,
                                  Q.end = Q.TimeSlotTimeOfDay $ T.TimeOfDay 16 0 0,
                                  Q.dayOfWeek = Just $ Q.TimeSlotDayOfWeek T.Friday
                                }
                            ],
                        Q.stages =
                          Just
                            Q.StageFilter
                              { Q.matchBothStages = False,
                                Q.stageIds = [1, 4]
                              },
                        Q.rules = Just [Q.ClamBlitz]
                      }
                  ]
              }
      let eventMatches =
            [ -- マッチする
              S.EventMatch
                { S.startTime = TU.createUTCTime 2021 1 1 4 0, -- 金曜日
                  S.endTime = TU.createUTCTime 2021 1 1 6 0,
                  S.rule = S.Rule {key = S.ClamBlitz, name = ""},
                  S.stages = [S.Stage 1 "" "", S.Stage 2 "" ""],
                  S.eventSummary = S.EventSummary "id" "イカダッシュバトル" "イカダッシュ速度アップ！ イカやタコでステージを泳ぎ回れ！",
                  S.isFest = False
                },
              -- マッチしない
              S.EventMatch
                { S.startTime = TU.createUTCTime 2021 1 2 6 0, -- 金曜日じゃない
                  S.endTime = TU.createUTCTime 2021 1 2 8 0,
                  S.rule = S.Rule {key = S.ClamBlitz, name = ""},
                  S.stages = [S.Stage 1 "" "", S.Stage 2 "" ""],
                  S.eventSummary = S.EventSummary "id" "イカダッシュバトル2" "イカダッシュ速度アップ！ イカやタコでステージを泳ぎ回れ！2",
                  S.isFest = False
                }
            ]
      let icalEvents = FI.createICalEventsFromEventMatches queryRoot eventMatches
      icalEvents
        `shouldBe` [ I.ICalEvent
                       { I.id = "3cd804136689020fd443d62339a32a5ca1375daa7be1327782050d0675bca85e",
                         I.summary = "イカダッシュバトル【ガチアサリ】イベントマッチ / ユノハナ大渓谷, ゴンズイ地区",
                         I.description = "イカダッシュ速度アップ！ イカやタコでステージを泳ぎ回れ！\n\n13:00から15:00までガチアサリの予定があります。\n・イベントマッチ\n・ステージ: ユノハナ大渓谷, ゴンズイ地区",
                         I.start = TU.createUTCTime 2021 1 1 4 0,
                         I.end = TU.createUTCTime 2021 1 1 6 0
                       }
                   ]

    it "正常系 英語" $ do
      let queryRoot =
            Q.QueryRoot
              { Q.language = Q.English,
                Q.utcOffset = Q.UtcOffsetTimeZone $ TU.createTimeZone 9 "",
                Q.filters =
                  [ Q.FilterCondition
                      { Q.modes = Just [Q.Event],
                        Q.timeSlots =
                          Just
                            [ Q.TimeSlot
                                { Q.start = Q.TimeSlotTimeOfDay $ T.TimeOfDay 12 0 0,
                                  Q.end = Q.TimeSlotTimeOfDay $ T.TimeOfDay 16 0 0,
                                  Q.dayOfWeek = Just $ Q.TimeSlotDayOfWeek T.Friday
                                }
                            ],
                        Q.stages =
                          Just
                            Q.StageFilter
                              { Q.matchBothStages = False,
                                Q.stageIds = [1, 4]
                              },
                        Q.rules = Just [Q.ClamBlitz]
                      }
                  ]
              }
      let eventMatches =
            [ -- マッチする
              S.EventMatch
                { S.startTime = TU.createUTCTime 2021 1 1 4 0, -- 金曜日
                  S.endTime = TU.createUTCTime 2021 1 1 6 0,
                  S.rule = S.Rule {key = S.ClamBlitz, name = ""},
                  S.stages = [S.Stage 1 "" "", S.Stage 2 "" ""],
                  S.eventSummary = S.EventSummary "id" "イカダッシュバトル" "イカダッシュ速度アップ！ イカやタコでステージを泳ぎ回れ！",
                  S.isFest = False
                }
            ]
      let icalEvents = FI.createICalEventsFromEventMatches queryRoot eventMatches
      icalEvents
        `shouldBe` [ I.ICalEvent
                       { I.id = "d220020d5918000e7063b463a61882defa847a9d509650d68e39228a5c36f408",
                         I.summary = "【Clam Blitz】Challenge / Scorch Gorge, Eeltail Alley / イカダッシュバトル",
                         I.description = "There is a scheduled Clam Blitz from 13:00 to 15:00.\n- Challenge\n- Stages: Scorch Gorge, Eeltail Alley\n\nイカダッシュ速度アップ！ イカやタコでステージを泳ぎ回れ！",
                         I.start = TU.createUTCTime 2021 1 1 4 0,
                         I.end = TU.createUTCTime 2021 1 1 6 0
                       }
                   ]
