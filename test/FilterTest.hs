module FilterTest (test) where

import qualified Query as Q
import qualified Data.Time as T
import qualified Filter as F
import Test.Hspec (describe, hspec, it, shouldBe)
import qualified TestUtil as TU
import Prelude (Bool (..), IO, Maybe (Just, Nothing), ($))

-- utc の 1~4
tod1 = Q.TimeSlotTimeOfDay $ T.TimeOfDay 21 0 0
tod2 = Q.TimeSlotTimeOfDay $ T.TimeOfDay 3 0 0
tod3 = Q.TimeSlotTimeOfDay $ T.TimeOfDay 23 0 0
tod4 = Q.TimeSlotTimeOfDay $ T.TimeOfDay 1 0 0

s1 = TU.createUTCTime 2021 1 1 21 0
s2 = TU.createUTCTime 2021 1 2 3 0
s3 = TU.createUTCTime 2021 1 1 23 0
s4 = TU.createUTCTime 2021 1 2 1 0

jst = Q.UtcOffsetTimeZone $ TU.createTimeZone 9 ""
utc = Q.UtcOffsetTimeZone $ TU.createTimeZone 0 ""

test :: IO ()
test = hspec $ do
  describe "inTimeSlot" $ do
    -- ============
    -- 時刻が範囲内かどうかを調べるのはA~Mの13パターンを考えればよい
    -- ============
    -- ss: schedule start
    -- se: schedule end
    -- ts: time slot start
    -- te: time slot end
    -- ============
    --  1| 3|  |4 |2
    -- 1: 2021-01-01(金) 21:00:00
    -- 2: 2021-01-02(土) 03:00:00
    -- 3: 2021-01-01(金) 23:00:00
    -- 4: 2021-01-02(土) 01:00:00
    -- ------------
    -- A. True
    --    ss|  |se
    -- ts|        |te
    -- B. True
    -- ss|        |se
    -- ts|        |te
    -- C. True
    --    ss|     |se
    -- ts|        |te
    -- D. True
    -- ss|     |se
    -- ts|        |te
    -- E. True
    -- ss|        |se
    --    ts|  |te
    -- F. True
    -- ss|        |se
    --    ts|     |te
    -- G. True
    -- ss|        |se
    -- ts|     |te
    -- H. True
    -- ss|     |se
    --    ts|     |te
    -- I. False
    -- ss|  |se
    --    ts|     |te
    -- J. False
    -- ss|  |se
    --       ts|  |te
    -- K. True
    --    ss|     |se
    -- ts|     |te
    -- L. False
    --    ss|     |se
    -- ts|  |te
    -- M. False
    --       ss|  |se
    -- ts|  |te

    it "Case A." $ do
      F.inTimeSlot s3 s4 utc (Q.TimeSlot tod1 tod2 Nothing) `shouldBe` True
    it "Case B." $ do
      F.inTimeSlot s1 s2 utc (Q.TimeSlot tod1 tod2 Nothing) `shouldBe` True
    it "Case C." $ do
      F.inTimeSlot s3 s2 utc (Q.TimeSlot tod1 tod2 Nothing) `shouldBe` True
    it "Case D." $ do
      F.inTimeSlot s1 s4 utc (Q.TimeSlot tod1 tod2 Nothing) `shouldBe` True
    it "Case E." $ do
      F.inTimeSlot s1 s2 utc (Q.TimeSlot tod3 tod4 Nothing) `shouldBe` True
    it "Case F." $ do
      F.inTimeSlot s1 s2 utc (Q.TimeSlot tod3 tod2 Nothing) `shouldBe` True
    it "Case G." $ do
      F.inTimeSlot s1 s2 utc (Q.TimeSlot tod1 tod4 Nothing) `shouldBe` True
    it "Case H." $ do
      F.inTimeSlot s1 s4 utc (Q.TimeSlot tod3 tod2 Nothing) `shouldBe` True
    it "Case I." $ do
      F.inTimeSlot s1 s3 utc (Q.TimeSlot tod3 tod4 Nothing) `shouldBe` False
    it "Case J." $ do
      F.inTimeSlot s1 s3 utc (Q.TimeSlot tod4 tod2 Nothing) `shouldBe` False
    it "Case K." $ do
      F.inTimeSlot s3 s2 utc (Q.TimeSlot tod1 tod4 Nothing) `shouldBe` True
    it "Case L." $ do
      F.inTimeSlot s3 s2 utc (Q.TimeSlot tod1 tod3 Nothing) `shouldBe` False
    it "Case M." $ do
      F.inTimeSlot s4 s2 utc (Q.TimeSlot tod1 tod3 Nothing) `shouldBe` False




    -- it "曜日指定はスケジュールの開始時刻を参照する" $ do
    --   let apiStartTime = TU.createUTCTime 2021 1 1 14 0 -- 日本では2021年1月1日23時 金曜日
    --   let apiEndTime = TU.createUTCTime 2021 1 1 16 0 -- 日本では2021年1月2日1時 土曜日
    --   let utcOffset = Q.UtcOffsetTimeZone $ TU.createTimeZone 9 ""
    --   let timeSlot = Q.TimeSlot {
    --     start = Q.TimeSlotTimeOfDay $ T.TimeOfDay 22 0 0,
    --     end = Q.TimeSlotTimeOfDay $ T.TimeOfDay 2 0 0,
    --     dayOfWeek = Just $ Q.TimeSlotDayOfWeek T.Friday
    --   }
    --   F.inTimeSlot apiStartTime apiEndTime utcOffset timeSlot `shouldBe` True
    --
    -- it "曜日指定はスケジュールの終了時刻を参照しない" $ do
    --   let apiStartTime = TU.createUTCTime 2021 1 1 4 0 -- 日本では2021年1月1日13時 金曜日
    --   let apiEndTime = TU.createUTCTime 2021 1 1 6 0 -- 日本では2021年1月1日15時
    --   let utcOffset = Q.UtcOffsetTimeZone $ TU.createTimeZone 9 ""
    --   let timeSlot = Q.TimeSlot {
    --     start = Q.TimeSlotTimeOfDay $ T.TimeOfDay 12 0 0,
    --     end = Q.TimeSlotTimeOfDay $ T.TimeOfDay 16 0 0,
    --     dayOfWeek = Just $ Q.TimeSlotDayOfWeek T.Friday
    --   }
    --   F.inTimeSlot apiStartTime apiEndTime utcOffset timeSlot `shouldBe` True
    --
    -- it "TimeSlotがスケジュールを完全に覆っていて、曜日指定がない" $ do
    --   let apiStartTime = TU.createUTCTime 2021 1 1 4 0 -- 日本では2021年1月1日13時
    --   let apiEndTime = TU.createUTCTime 2021 1 1 6 0 -- 日本では2021年1月1日15時
    --   let utcOffset = Q.UtcOffsetTimeZone $ TU.createTimeZone 9 ""
    --   let timeSlot = Q.TimeSlot {
    --     start = Q.TimeSlotTimeOfDay $ T.TimeOfDay 12 0 0,
    --     end = Q.TimeSlotTimeOfDay $ T.TimeOfDay 16 0 0,
    --     dayOfWeek = Nothing
    --   }
    --   F.inTimeSlot apiStartTime apiEndTime utcOffset timeSlot `shouldBe` True
    --
    -- it "TimeSlotがスケジュールを完全に覆っていて、曜日指定がある" $ do
    --   let apiStartTime = TU.createUTCTime 2021 1 1 4 0 -- 日本では2021年1月1日13時 金曜日
    --   let apiEndTime = TU.createUTCTime 2021 1 1 6 0 -- 日本では2021年1月1日15時
    --   let utcOffset = Q.UtcOffsetTimeZone $ TU.createTimeZone 9 ""
    --   let timeSlot = Q.TimeSlot {
    --     start = Q.TimeSlotTimeOfDay $ T.TimeOfDay 12 0 0,
    --     end = Q.TimeSlotTimeOfDay $ T.TimeOfDay 16 0 0,
    --     dayOfWeek = Just $ Q.TimeSlotDayOfWeek T.Friday
    --   }
    --   F.inTimeSlot apiStartTime apiEndTime utcOffset timeSlot `shouldBe` True
