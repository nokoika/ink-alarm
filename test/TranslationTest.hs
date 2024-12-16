module TranslationTest (test) where

import qualified Query as Q
import qualified SplaApi as S
import qualified TestUtil as TU
import Test.Hspec (describe, hspec, it, shouldBe)
import qualified Translation (showCalendarDescription, showCalendarSummary)
import Prelude (IO, ($))

test :: IO ()
test = hspec $ do
  describe "showCalendarSummary" $ do
    it "returns a summary of the calendar" $ do
      Translation.showCalendarSummary
        Q.Japanese
        Q.Regular
        (S.Rule S.TurfWar "")
        [S.Stage {S.id = 1, S.name = "", S.image = ""}, S.Stage {S.id = 2, S.name = "", S.image = ""}]
        `shouldBe` "【ナワバリバトル】レギュラーマッチ / ユノハナ大渓谷, ゴンズイ地区"

  describe "showCalendarDescription" $ do
    it "returns a description of the japanese calendar" $ do
      Translation.showCalendarDescription
        Q.Japanese
        Q.Regular
        (S.Rule S.TurfWar "")
        [S.Stage {S.id = 1, S.name = "", S.image = ""}, S.Stage {S.id = 2, S.name = "", S.image = ""}]
        (TU.createZonedTime (2021, 1, 1, 21, 0) (9, ""), TU.createZonedTime (2021, 1, 1, 23, 0) (9, ""))
        `shouldBe`
        "21:00から23:00までナワバリバトルの予定があります。\n・レギュラーマッチ\n・ステージ: ユノハナ大渓谷, ゴンズイ地区"

    it "returns a description of the english calendar" $ do
      Translation.showCalendarDescription
        Q.English
        Q.Regular
        (S.Rule S.TurfWar "")
        [S.Stage {S.id = 1, S.name = "", S.image = ""}, S.Stage {S.id = 2, S.name = "", S.image = ""}]
        (TU.createZonedTime (2021, 1, 1, 21, 0) (9, ""), TU.createZonedTime (2021, 1, 1, 23, 0) (9, ""))
        `shouldBe`
        "There is a scheduled Turf War from 21:00 to 23:00.\n- Regular Battle\n- Stages: Scorch Gorge, Eeltail Alley"
