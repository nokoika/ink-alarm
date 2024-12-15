module FilterTest (test) where

import qualified Query as Q
import qualified SplaApi as S
import qualified Data.Time as T
import qualified Filter as F
import Test.Hspec (describe, hspec, it, shouldBe)
import qualified TestUtil as TU
import Prelude (Bool (..), IO, Maybe (Just, Nothing), ($), Int)

test :: IO ()
test = hspec $ do
  describe "inTimeSlot" $ do
    it "API: [13:00 (金), 15:00(金)), TimeSlot: [12:00, 16:00), 金 のとき、マッチする" $ do
      let apiStartTime = TU.createUTCTime 2021 1 1 4 0 -- 日本では2021年1月1日13時 金曜日
      let apiEndTime = TU.createUTCTime 2021 1 1 6 0 -- 日本では2021年1月1日15時
      let utcOffset = Q.UtcOffsetTimeZone $ TU.createTimeZone 9 ""
      let timeSlot = Q.TimeSlot {
        start = Q.TimeSlotTimeOfDay $ T.TimeOfDay 12 0 0,
        end = Q.TimeSlotTimeOfDay $ T.TimeOfDay 16 0 0,
        dayOfWeek = Just $ Q.TimeSlotDayOfWeek T.Friday
      }
      F.inTimeSlot apiStartTime apiEndTime utcOffset timeSlot `shouldBe` True
    it "API: [23:00 (金), 01:00(土)), TimeSlot: [16:00, 00:00), 金 のとき、マッチする" $ do
      let apiStartTime = TU.createUTCTime 2021 1 1 14 0 -- 日本では2021年1月1日23時 金曜日
      let apiEndTime = TU.createUTCTime 2021 1 1 16 0 -- 日本では2021年1月2日1時 土曜日
      let utcOffset = Q.UtcOffsetTimeZone $ TU.createTimeZone 9 ""
      let timeSlot = Q.TimeSlot {
        start = Q.TimeSlotTimeOfDay $ T.TimeOfDay 16 0 0,
        end = Q.TimeSlotTimeOfDay $ T.TimeOfDay 0 0 0,
        dayOfWeek = Just $ Q.TimeSlotDayOfWeek T.Friday
      }
      F.inTimeSlot apiStartTime apiEndTime utcOffset timeSlot `shouldBe` True
    it "API: [23:00 (金), 01:00(土)), TimeSlot: [00:00, 01:00), 土 のとき、マッチする" $ do
      let apiStartTime = TU.createUTCTime 2021 1 1 14 0 -- 日本では2021年1月1日23時 金曜日
      let apiEndTime = TU.createUTCTime 2021 1 1 16 0 -- 日本では2021年1月2日1時 土曜日
      let utcOffset = Q.UtcOffsetTimeZone $ TU.createTimeZone 9 ""
      let timeSlot = Q.TimeSlot {
        start = Q.TimeSlotTimeOfDay $ T.TimeOfDay 0 0 0,
        end = Q.TimeSlotTimeOfDay $ T.TimeOfDay 1 0 0,
        dayOfWeek = Just $ Q.TimeSlotDayOfWeek T.Saturday
      }
      F.inTimeSlot apiStartTime apiEndTime utcOffset timeSlot `shouldBe` True
    it "API: [13:00 (金), 15:00(金)), TimeSlot: [12:00, 16:00), 土 のとき、マッチしない" $ do
      let apiStartTime = TU.createUTCTime 2021 1 1 4 0 -- 日本では2021年1月1日13時 金曜日
      let apiEndTime = TU.createUTCTime 2021 1 1 6 0 -- 日本では2021年1月1日15時
      let utcOffset = Q.UtcOffsetTimeZone $ TU.createTimeZone 9 ""
      let timeSlot = Q.TimeSlot {
        start = Q.TimeSlotTimeOfDay $ T.TimeOfDay 12 0 0,
        end = Q.TimeSlotTimeOfDay $ T.TimeOfDay 16 0 0,
        dayOfWeek = Just $ Q.TimeSlotDayOfWeek T.Saturday
      }
      F.inTimeSlot apiStartTime apiEndTime utcOffset timeSlot `shouldBe` False

  describe "inStage" $ do
    let createStage :: Int -> S.Stage
        createStage id = S.Stage id "" ""
    it "API: [1, 2], StageFilter: [1, 2, 3, 4], matchBothStages: False のとき、マッチする" $ do
      let apiStages = [createStage 1, createStage 2]
      let stageFilter = Q.StageFilter {
        matchBothStages = False,
        stageIds = [1, 2]
      }
      F.inStage apiStages stageFilter `shouldBe` True
    it "API: [1, 2], StageFilter: [1, 3, 4], matchBothStages: True のとき、マッチしない" $ do
      let apiStages = [createStage 1, createStage 2]
      let stageFilter = Q.StageFilter {
        matchBothStages = True,
        stageIds = [1, 3, 4]
      }
      F.inStage apiStages stageFilter `shouldBe` False
    it "API: [1, 2], StageFilter: [1, 3, 4], matchBothStages: False のとき、マッチする" $ do
      let apiStages = [createStage 1, createStage 2]
      let stageFilter = Q.StageFilter {
        matchBothStages = False,
        stageIds = [1, 3, 4]
      }
      F.inStage apiStages stageFilter `shouldBe` True
    it "API: [1, 2], StageFilter: [] のとき、マッチしない" $ do
      let apiStages = [createStage 1, createStage 2]
      let stageFilter = Q.StageFilter {
        matchBothStages = False,
        stageIds = []
      }
      F.inStage apiStages stageFilter `shouldBe` False
    it "API: [], StageFilter: [1, 2, 3, 4] のとき、マッチしない" $ do
      let apiStages = []
      let stageFilter = Q.StageFilter {
        matchBothStages = False,
        stageIds = [1, 2, 3, 4]
      }
      F.inStage apiStages stageFilter `shouldBe` False
