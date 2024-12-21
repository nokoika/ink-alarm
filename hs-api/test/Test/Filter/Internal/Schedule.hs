module Test.Filter.Internal.Schedule (test) where

import qualified Data.Time as T
import qualified Filter.Internal.Schedule as FS
import qualified Query as Q
import qualified SplaApi as S
import Test.Hspec (describe, hspec, it, shouldBe)
import qualified TestUtil as TU
import Prelude (Bool (..), IO, Int, Maybe (Just, Nothing), ($))

test :: IO ()
test = hspec $ do
  describe "inTimeSlot" $ do
    it "API: [13:00 (金), 15:00(金)), TimeSlot: [12:00, 16:00), 金 のとき、マッチする" $ do
      let apiStartTime = TU.createUTCTime 2021 1 1 4 0 -- 日本では2021年1月1日13時 金曜日
      let apiEndTime = TU.createUTCTime 2021 1 1 6 0 -- 日本では2021年1月1日15時
      let utcOffset = TU.createTimeZone 9 ""
      let timeSlot =
            Q.TimeSlot
              { start = Q.TimeSlotTimeOfDay $ T.TimeOfDay 12 0 0,
                end = Q.TimeSlotTimeOfDay $ T.TimeOfDay 16 0 0,
                dayOfWeek = Just $ Q.TimeSlotDayOfWeek T.Friday
              }
      FS.inTimeSlot apiStartTime apiEndTime utcOffset timeSlot `shouldBe` True
    it "API: [23:00 (金), 01:00(土)), TimeSlot: [16:00, 00:00), 金 のとき、マッチする" $ do
      let apiStartTime = TU.createUTCTime 2021 1 1 14 0 -- 日本では2021年1月1日23時 金曜日
      let apiEndTime = TU.createUTCTime 2021 1 1 16 0 -- 日本では2021年1月2日1時 土曜日
      let utcOffset = TU.createTimeZone 9 ""
      let timeSlot =
            Q.TimeSlot
              { start = Q.TimeSlotTimeOfDay $ T.TimeOfDay 16 0 0,
                end = Q.TimeSlotTimeOfDay $ T.TimeOfDay 0 0 0,
                dayOfWeek = Just $ Q.TimeSlotDayOfWeek T.Friday
              }
      FS.inTimeSlot apiStartTime apiEndTime utcOffset timeSlot `shouldBe` True
    it "API: [23:00 (金), 01:00(土)), TimeSlot: [00:00, 01:00), 土 のとき、マッチする" $ do
      let apiStartTime = TU.createUTCTime 2021 1 1 14 0 -- 日本では2021年1月1日23時 金曜日
      let apiEndTime = TU.createUTCTime 2021 1 1 16 0 -- 日本では2021年1月2日1時 土曜日
      let utcOffset = TU.createTimeZone 9 ""
      let timeSlot =
            Q.TimeSlot
              { start = Q.TimeSlotTimeOfDay $ T.TimeOfDay 0 0 0,
                end = Q.TimeSlotTimeOfDay $ T.TimeOfDay 1 0 0,
                dayOfWeek = Just $ Q.TimeSlotDayOfWeek T.Saturday
              }
      FS.inTimeSlot apiStartTime apiEndTime utcOffset timeSlot `shouldBe` True
    it "API: [13:00 (金), 15:00(金)), TimeSlot: [12:00, 16:00), 土 のとき、マッチしない" $ do
      let apiStartTime = TU.createUTCTime 2021 1 1 4 0 -- 日本では2021年1月1日13時 金曜日
      let apiEndTime = TU.createUTCTime 2021 1 1 6 0 -- 日本では2021年1月1日15時
      let utcOffset = TU.createTimeZone 9 ""
      let timeSlot =
            Q.TimeSlot
              { start = Q.TimeSlotTimeOfDay $ T.TimeOfDay 12 0 0,
                end = Q.TimeSlotTimeOfDay $ T.TimeOfDay 16 0 0,
                dayOfWeek = Just $ Q.TimeSlotDayOfWeek T.Saturday
              }
      FS.inTimeSlot apiStartTime apiEndTime utcOffset timeSlot `shouldBe` False
    it "API: [13:00 (金), 15:00(金)), TimeSlot: [00:00, 00:00), 金 のとき、マッチする" $ do
      let apiStartTime = TU.createUTCTime 2021 1 1 4 0 -- 日本では2021年1月1日13時 金曜日
      let apiEndTime = TU.createUTCTime 2021 1 1 6 0 -- 日本では2021年1月1日15時
      let utcOffset = TU.createTimeZone 9 ""
      let timeSlot =
            Q.TimeSlot
              { start = Q.TimeSlotTimeOfDay $ T.TimeOfDay 0 0 0,
                end = Q.TimeSlotTimeOfDay $ T.TimeOfDay 0 0 0,
                dayOfWeek = Just $ Q.TimeSlotDayOfWeek T.Friday
              }
      FS.inTimeSlot apiStartTime apiEndTime utcOffset timeSlot `shouldBe` False

  describe "inStage" $ do
    let createStage :: Int -> S.Stage
        createStage id = S.Stage id "" ""
    it "API: [1, 2], StageFilter: [1, 2, 3, 4], matchBothStages: False のとき、マッチする" $ do
      let apiStages = [createStage 1, createStage 2]
      let stageFilter =
            Q.StageFilter
              { matchBothStages = False,
                stageIds = [1, 2]
              }
      FS.inStage apiStages stageFilter `shouldBe` True
    it "API: [1, 2], StageFilter: [1, 3, 4], matchBothStages: True のとき、マッチしない" $ do
      let apiStages = [createStage 1, createStage 2]
      let stageFilter =
            Q.StageFilter
              { matchBothStages = True,
                stageIds = [1, 3, 4]
              }
      FS.inStage apiStages stageFilter `shouldBe` False
    it "API: [1, 2], StageFilter: [1, 3, 4], matchBothStages: False のとき、マッチする" $ do
      let apiStages = [createStage 1, createStage 2]
      let stageFilter =
            Q.StageFilter
              { matchBothStages = False,
                stageIds = [1, 3, 4]
              }
      FS.inStage apiStages stageFilter `shouldBe` True
    it "API: [1, 2], StageFilter: [] のとき、マッチしない" $ do
      let apiStages = [createStage 1, createStage 2]
      let stageFilter =
            Q.StageFilter
              { matchBothStages = False,
                stageIds = []
              }
      FS.inStage apiStages stageFilter `shouldBe` False
    it "API: [], StageFilter: [1, 2, 3, 4] のとき、マッチしない" $ do
      let apiStages = []
      let stageFilter =
            Q.StageFilter
              { matchBothStages = False,
                stageIds = [1, 2, 3, 4]
              }
      FS.inStage apiStages stageFilter `shouldBe` False
    it "API: [], StageFilter: [] のとき、マッチしない" $ do
      let apiStages = []
      let stageFilter =
            Q.StageFilter
              { matchBothStages = False,
                stageIds = []
              }
      FS.inStage apiStages stageFilter `shouldBe` False

  describe "inRules" $ do
    it "API Rule: アサリ, Query Rule: アサリ のとき、マッチする" $ do
      let apiRule = S.Rule {key = S.ClamBlitz, name = ""}
      let rules = [Q.ClamBlitz]
      FS.inRules apiRule rules `shouldBe` True
    it "API Rule: アサリ, Query Rule: ヤグラ,アサリ のとき、マッチする" $ do
      let apiRule = S.Rule {key = S.ClamBlitz, name = ""}
      let rules = [Q.TowerControl, Q.ClamBlitz]
      FS.inRules apiRule rules `shouldBe` True
    it "API Rule: アサリ, Query Rule: ナワバリ のとき、マッチしない" $ do
      let apiRule = S.Rule {key = S.ClamBlitz, name = ""}
      let rules = [Q.TurfWar]
      FS.inRules apiRule rules `shouldBe` False
    it "API Rule: アサリ, Query Rule: [] のとき、マッチしない" $ do
      let apiRule = S.Rule {key = S.ClamBlitz, name = ""}
      let rules = []
      FS.inRules apiRule rules `shouldBe` False
    it "API Rule: [], Query Rule: アサリ のとき、マッチしない" $ do
      let apiRule = S.Rule {key = S.ClamBlitz, name = ""}
      let rules = [Q.ClamBlitz]
      FS.inRules apiRule rules `shouldBe` True
    it "API Rule: [], Query Rule: [] のとき、マッチしない" $ do
      let apiRule = S.Rule {key = S.ClamBlitz, name = ""}
      let rules = []
      FS.inRules apiRule rules `shouldBe` False

  describe "filterDefaultSchedule" $ do
    it "条件全指定 マッチ" $ do
      let defaultSchedule =
            S.DefaultSchedule
              { S.startTime = TU.createUTCTime 2021 1 1 4 0,
                S.endTime = TU.createUTCTime 2021 1 1 6 0,
                S.rule = Just $ S.Rule {key = S.ClamBlitz, name = ""},
                S.stages = Just [S.Stage 1 "" "", S.Stage 2 "" ""],
                S.isFest = False
              }
      let query =
            Q.FilterCondition
              { Q.matchType = Q.BankaraChallenge,
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
                Q.rules = Just [Q.ClamBlitz],
                Q.notifications = Just [Q.NotificationSetting 10, Q.NotificationSetting 20]
              }
      FS.filterDefaultSchedule query defaultSchedule (TU.createTimeZone 9 "") Q.BankaraChallenge `shouldBe` True

    it "条件全指定 ステージがマッチしない" $ do
      let defaultSchedule =
            S.DefaultSchedule
              { S.startTime = TU.createUTCTime 2021 1 1 4 0,
                S.endTime = TU.createUTCTime 2021 1 1 6 0,
                S.rule = Just $ S.Rule {key = S.ClamBlitz, name = ""},
                S.stages = Just [S.Stage 1 "" "", S.Stage 2 "" ""],
                S.isFest = False
              }
      let query =
            Q.FilterCondition
              { Q.matchType = Q.BankaraChallenge,
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
                      { Q.matchBothStages = True, -- 片方しかマッチしてないから結果は False になるはず
                        Q.stageIds = [1, 4]
                      },
                Q.rules = Just [Q.ClamBlitz],
                Q.notifications = Just [Q.NotificationSetting 10, Q.NotificationSetting 20]
              }
      FS.filterDefaultSchedule query defaultSchedule (TU.createTimeZone 9 "") Q.BankaraChallenge `shouldBe` False

    it "条件全指定 ルールがマッチしない" $ do
      let defaultSchedule =
            S.DefaultSchedule
              { S.startTime = TU.createUTCTime 2021 1 1 4 0,
                S.endTime = TU.createUTCTime 2021 1 1 6 0,
                S.rule = Just $ S.Rule {key = S.ClamBlitz, name = ""},
                S.stages = Just [S.Stage 1 "" "", S.Stage 2 "" ""],
                S.isFest = False
              }
      let query =
            Q.FilterCondition
              { Q.matchType = Q.BankaraChallenge,
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
                Q.rules = Just [Q.TowerControl], -- マッチしないルール
                Q.notifications = Just [Q.NotificationSetting 10, Q.NotificationSetting 20]
              }
      FS.filterDefaultSchedule query defaultSchedule (TU.createTimeZone 9 "") Q.BankaraChallenge `shouldBe` False

    it "条件全指定 時間帯がマッチしない" $ do
      let defaultSchedule =
            S.DefaultSchedule
              { S.startTime = TU.createUTCTime 2021 1 1 4 0,
                S.endTime = TU.createUTCTime 2021 1 1 6 0,
                S.rule = Just $ S.Rule {key = S.ClamBlitz, name = ""},
                S.stages = Just [S.Stage 1 "" "", S.Stage 2 "" ""],
                S.isFest = False
              }
      let query =
            Q.FilterCondition
              { Q.matchType = Q.BankaraChallenge,
                Q.timeSlots =
                  Just
                    [ Q.TimeSlot
                        { Q.start = Q.TimeSlotTimeOfDay $ T.TimeOfDay 12 0 0,
                          Q.end = Q.TimeSlotTimeOfDay $ T.TimeOfDay 16 0 0,
                          Q.dayOfWeek = Just $ Q.TimeSlotDayOfWeek T.Saturday -- 金曜日じゃない
                        }
                    ],
                Q.stages =
                  Just
                    Q.StageFilter
                      { Q.matchBothStages = False,
                        Q.stageIds = [1, 4]
                      },
                Q.rules = Just [Q.ClamBlitz],
                Q.notifications = Just [Q.NotificationSetting 10, Q.NotificationSetting 20]
              }
      FS.filterDefaultSchedule query defaultSchedule (TU.createTimeZone 9 "") Q.BankaraChallenge `shouldBe` False

    it "条件全指定 フェスの場合はマッチしない" $ do
      let defaultSchedule =
            S.DefaultSchedule
              { S.startTime = TU.createUTCTime 2021 1 1 4 0,
                S.endTime = TU.createUTCTime 2021 1 1 6 0,
                S.rule = Just $ S.Rule {key = S.ClamBlitz, name = ""},
                S.stages = Just [S.Stage 1 "" "", S.Stage 2 "" ""],
                S.isFest = True
              }
      let query =
            Q.FilterCondition
              { Q.matchType = Q.BankaraChallenge,
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
                Q.rules = Just [Q.ClamBlitz],
                Q.notifications = Just [Q.NotificationSetting 10, Q.NotificationSetting 20]
              }
      FS.filterDefaultSchedule query defaultSchedule (TU.createTimeZone 9 "") Q.BankaraChallenge `shouldBe` False

    it "条件全指定 マッチタイプが違う" $ do
      let defaultSchedule =
            S.DefaultSchedule
              { S.startTime = TU.createUTCTime 2021 1 1 4 0,
                S.endTime = TU.createUTCTime 2021 1 1 6 0,
                S.rule = Just $ S.Rule {key = S.ClamBlitz, name = ""},
                S.stages = Just [S.Stage 1 "" "", S.Stage 2 "" ""],
                S.isFest = False
              }
      let query =
            Q.FilterCondition
              { Q.matchType = Q.BankaraChallenge,
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
                Q.rules = Just [Q.ClamBlitz],
                Q.notifications = Just [Q.NotificationSetting 10, Q.NotificationSetting 20]
              }
      FS.filterDefaultSchedule query defaultSchedule (TU.createTimeZone 9 "") Q.XMatch `shouldBe` False

    it "条件最小限 マッチ" $ do
      let defaultSchedule =
            S.DefaultSchedule
              { S.startTime = TU.createUTCTime 2021 1 1 4 0,
                S.endTime = TU.createUTCTime 2021 1 1 6 0,
                S.rule = Just $ S.Rule {key = S.ClamBlitz, name = ""},
                S.stages = Just [S.Stage 1 "" "", S.Stage 2 "" ""],
                S.isFest = False
              }
      let query =
            Q.FilterCondition
              { Q.matchType = Q.BankaraChallenge,
                Q.timeSlots = Nothing,
                Q.stages = Nothing,
                Q.rules = Nothing,
                Q.notifications = Nothing
              }
      FS.filterDefaultSchedule query defaultSchedule (TU.createTimeZone 9 "") Q.BankaraChallenge `shouldBe` True

    it "条件最小限 マッチしない" $ do
      let defaultSchedule =
            S.DefaultSchedule
              { S.startTime = TU.createUTCTime 2021 1 1 4 0,
                S.endTime = TU.createUTCTime 2021 1 1 6 0,
                S.rule = Just $ S.Rule {key = S.ClamBlitz, name = ""},
                S.stages = Just [S.Stage 1 "" "", S.Stage 2 "" ""],
                S.isFest = False
              }
      let query =
            Q.FilterCondition
              { Q.matchType = Q.BankaraChallenge,
                Q.timeSlots = Nothing,
                Q.stages = Nothing,
                Q.rules = Nothing,
                Q.notifications = Nothing
              }
      FS.filterDefaultSchedule query defaultSchedule (TU.createTimeZone 9 "") Q.XMatch `shouldBe` False

  describe "filterEventMatch" $ do
    it "条件全指定 マッチ" $ do
      let eventMatch =
            S.EventMatch
              { S.startTime = TU.createUTCTime 2021 1 1 4 0,
                S.endTime = TU.createUTCTime 2021 1 1 6 0,
                S.rule = S.Rule {key = S.ClamBlitz, name = ""},
                S.stages = [S.Stage 1 "" "", S.Stage 2 "" ""],
                S.eventSummary = S.EventSummary "id" "name" "desc",
                S.isFest = False
              }
      let query =
            Q.FilterCondition
              { Q.matchType = Q.Event,
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
                Q.rules = Just [Q.ClamBlitz],
                Q.notifications = Just [Q.NotificationSetting 10, Q.NotificationSetting 20]
              }
      FS.filterEventMatch query eventMatch (TU.createTimeZone 9 "") `shouldBe` True

    it "条件全指定 ステージがマッチしない" $ do
      let eventMatch =
            S.EventMatch
              { S.startTime = TU.createUTCTime 2021 1 1 4 0,
                S.endTime = TU.createUTCTime 2021 1 1 6 0,
                S.rule = S.Rule {key = S.ClamBlitz, name = ""},
                S.stages = [S.Stage 1 "" "", S.Stage 2 "" ""],
                S.eventSummary = S.EventSummary "id" "name" "desc",
                S.isFest = False
              }
      let query =
            Q.FilterCondition
              { Q.matchType = Q.Event,
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
                      { Q.matchBothStages = True, -- 片方しかマッチしてないから結果は False になるはず
                        Q.stageIds = [1, 4]
                      },
                Q.rules = Just [Q.ClamBlitz],
                Q.notifications = Just [Q.NotificationSetting 10, Q.NotificationSetting 20]
              }
      FS.filterEventMatch query eventMatch (TU.createTimeZone 9 "") `shouldBe` False

    it "条件全指定 ルールがマッチしない" $ do
      let eventMatch =
            S.EventMatch
              { S.startTime = TU.createUTCTime 2021 1 1 4 0,
                S.endTime = TU.createUTCTime 2021 1 1 6 0,
                S.rule = S.Rule {key = S.ClamBlitz, name = ""},
                S.stages = [S.Stage 1 "" "", S.Stage 2 "" ""],
                S.eventSummary = S.EventSummary "id" "name" "desc",
                S.isFest = False
              }
      let query =
            Q.FilterCondition
              { Q.matchType = Q.Event,
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
                Q.rules = Just [Q.TowerControl], -- マッチしないルール
                Q.notifications = Just [Q.NotificationSetting 10, Q.NotificationSetting 20]
              }
      FS.filterEventMatch query eventMatch (TU.createTimeZone 9 "") `shouldBe` False

    it "条件全指定 時間帯がマッチしない" $ do
      let eventMatch =
            S.EventMatch
              { S.startTime = TU.createUTCTime 2021 1 1 4 0,
                S.endTime = TU.createUTCTime 2021 1 1 6 0,
                S.rule = S.Rule {key = S.ClamBlitz, name = ""},
                S.stages = [S.Stage 1 "" "", S.Stage 2 "" ""],
                S.eventSummary = S.EventSummary "id" "name" "desc",
                S.isFest = False
              }
      let query =
            Q.FilterCondition
              { Q.matchType = Q.Event,
                Q.timeSlots =
                  Just
                    [ Q.TimeSlot
                        { Q.start = Q.TimeSlotTimeOfDay $ T.TimeOfDay 12 0 0,
                          Q.end = Q.TimeSlotTimeOfDay $ T.TimeOfDay 16 0 0,
                          Q.dayOfWeek = Just $ Q.TimeSlotDayOfWeek T.Saturday -- 金曜日じゃない
                        }
                    ],
                Q.stages =
                  Just
                    Q.StageFilter
                      { Q.matchBothStages = False,
                        Q.stageIds = [1, 4]
                      },
                Q.rules = Just [Q.ClamBlitz],
                Q.notifications = Just [Q.NotificationSetting 10, Q.NotificationSetting 20]
              }
      FS.filterEventMatch query eventMatch (TU.createTimeZone 9 "") `shouldBe` False

    it "条件全指定 フェスの場合はマッチしない" $ do
      let eventMatch =
            S.EventMatch
              { S.startTime = TU.createUTCTime 2021 1 1 4 0,
                S.endTime = TU.createUTCTime 2021 1 1 6 0,
                S.rule = S.Rule {key = S.ClamBlitz, name = ""},
                S.stages = [S.Stage 1 "" "", S.Stage 2 "" ""],
                S.eventSummary = S.EventSummary "id" "name" "desc",
                S.isFest = True
              }
      let query =
            Q.FilterCondition
              { Q.matchType = Q.Event,
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
                Q.rules = Just [Q.ClamBlitz],
                Q.notifications = Just [Q.NotificationSetting 10, Q.NotificationSetting 20]
              }
      FS.filterEventMatch query eventMatch (TU.createTimeZone 9 "") `shouldBe` False

    it "条件全指定 マッチタイプが違う" $ do
      let eventMatch =
            S.EventMatch
              { S.startTime = TU.createUTCTime 2021 1 1 4 0,
                S.endTime = TU.createUTCTime 2021 1 1 6 0,
                S.rule = S.Rule {key = S.ClamBlitz, name = ""},
                S.stages = [S.Stage 1 "" "", S.Stage 2 "" ""],
                S.eventSummary = S.EventSummary "id" "name" "desc",
                S.isFest = False
              }
      let query =
            Q.FilterCondition
              { Q.matchType = Q.BankaraChallenge,
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
                Q.rules = Just [Q.ClamBlitz],
                Q.notifications = Just [Q.NotificationSetting 10, Q.NotificationSetting 20]
              }
      FS.filterEventMatch query eventMatch (TU.createTimeZone 9 "") `shouldBe` False

    it "条件最小限 マッチ" $ do
      let eventMatch =
            S.EventMatch
              { S.startTime = TU.createUTCTime 2021 1 1 4 0,
                S.endTime = TU.createUTCTime 2021 1 1 6 0,
                S.rule = S.Rule {key = S.ClamBlitz, name = ""},
                S.stages = [S.Stage 1 "" "", S.Stage 2 "" ""],
                S.eventSummary = S.EventSummary "id" "name" "desc",
                S.isFest = False
              }
      let query =
            Q.FilterCondition
              { Q.matchType = Q.Event,
                Q.timeSlots = Nothing,
                Q.stages = Nothing,
                Q.rules = Nothing,
                Q.notifications = Nothing
              }
      FS.filterEventMatch query eventMatch (TU.createTimeZone 9 "") `shouldBe` True