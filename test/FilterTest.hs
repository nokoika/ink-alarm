module FilterTest (test) where

import qualified Query as Q
import qualified Data.Time as T
import qualified Filter as F
import Test.Hspec (describe, hspec, it, shouldBe)
import qualified TestUtil as TU
import Prelude (Bool (..), IO, Maybe (Just, Nothing), ($))

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

    --
    --
    -- it "曜日指定はスケジュールの開始時刻を参照する" $ do
    --   let apiStartTime = TU.createUTCTime 2021 1 1 14 0 -- 日本では2021年1月1日23時 金曜日
    --   let apiEndTime = TU.createUTCTime 2021 1 1 16 0 -- 日本では2021年1月2日1時 土曜日
    --   let utcOffset = Q.UtcOffsetTimeZone $ TU.createTimeZone 9 ""
    --   let timeSlot1 = Q.TimeSlot {
    --     start = Q.TimeSlotTimeOfDay $ T.TimeOfDay 22 0 0,
    --     end = Q.TimeSlotTimeOfDay $ T.TimeOfDay 2 0 0,
    --     dayOfWeek = Just $ Q.TimeSlotDayOfWeek T.Friday
    --   }
    --   F.inTimeSlot apiStartTime apiEndTime utcOffset timeSlot1 `shouldBe` True
    --   let timeSlot2 = Q.TimeSlot {
    --     start = Q.TimeSlotTimeOfDay $ T.TimeOfDay 22 0 0,
    --     end = Q.TimeSlotTimeOfDay $ T.TimeOfDay 2 0 0,
    --     dayOfWeek = Just $ Q.TimeSlotDayOfWeek T.Saturday
    --   }
    --   F.inTimeSlot apiStartTime apiEndTime utcOffset timeSlot2 `shouldBe` False
    --
    -- it "曜日指定はスケジュールの終了時刻を参照しない" $ do
    --   let apiStartTime = TU.createUTCTime 2021 1 1 4 0 -- 日本では2021年1月1日13時 金曜日
    --   let apiEndTime = TU.createUTCTime 2021 1 1 6 0 -- 日本では2021年1月1日15時
    --   let utcOffset = Q.UtcOffsetTimeZone $ TU.createTimeZone 9 ""
    --   let timeSlot1 = Q.TimeSlot {
    --     start = Q.TimeSlotTimeOfDay $ T.TimeOfDay 12 0 0,
    --     end = Q.TimeSlotTimeOfDay $ T.TimeOfDay 16 0 0,
    --     dayOfWeek = Just $ Q.TimeSlotDayOfWeek T.Friday
    --   }
    --   F.inTimeSlot apiStartTime apiEndTime utcOffset timeSlot1 `shouldBe` True
    --   let timeSlot2 = Q.TimeSlot {
    --     start = Q.TimeSlotTimeOfDay $ T.TimeOfDay 12 0 0,
    --     end = Q.TimeSlotTimeOfDay $ T.TimeOfDay 16 0 0,
    --     dayOfWeek = Just $ Q.TimeSlotDayOfWeek T.Saturday
    --   }
    --   F.inTimeSlot apiStartTime apiEndTime utcOffset timeSlot2 `shouldBe` False
