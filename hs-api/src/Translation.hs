module Translation (showCalendarSummary, showCalendarDescription, showApplicationName, showICalProdId) where

import Data.Aeson (Value (String))
import Data.List (intercalate)
import qualified Data.Maybe as M
import qualified Data.Time as T
import qualified Data.Time.Calendar as C
import qualified Query as Q
import qualified SplaApi as S
import Prelude (Maybe (Just, Nothing), Show (show), String, map, ($), (++), (.))

showApplicationName :: Q.Language -> String
showApplicationName Q.Japanese = "ガチアラーム"
showApplicationName Q.English = "Ink Alarm"

-- PRODID:-//組織名//製品名//言語
showICalProdId :: Q.Language -> String
showICalProdId Q.Japanese = "-//nokoika//" ++ showApplicationName Q.Japanese ++ " github.com/nokoika/ink-alarm//JA"
showICalProdId Q.English = "-//nokoika//" ++ showApplicationName Q.English ++ " github.com/nokoika/ink-alarm//EN"

showStageName :: Q.Language -> S.Stage -> String
showStageName Q.Japanese S.Stage {id} = case id of
  1 -> "ユノハナ大渓谷"
  2 -> "ゴンズイ地区"
  3 -> "ヤガラ市場"
  4 -> "マテガイ放水路"
  5 -> "ナンプラー遺跡"
  6 -> "ナメロウ金属"
  7 -> "クサヤ温泉"
  8 -> "タラポートショッピングパーク"
  9 -> "ヒラメが丘団地"
  10 -> "マサバ海峡大橋"
  11 -> "キンメダイ美術館"
  12 -> "マヒマヒリゾート＆スパ"
  13 -> "海女美術大学"
  14 -> "チョウザメ造船"
  15 -> "ザトウマーケット"
  16 -> "スメーシーワールド"
  17 -> "コンブトラック"
  18 -> "マンタマリア号"
  19 -> "タカアシ経済特区"
  20 -> "オヒョウ海運"
  21 -> "バイガイ亭"
  22 -> "ネギトロ炭鉱"
  23 -> "カジキ空港"
  24 -> "リュウグウターミナル"
  _ -> ""
showStageName Q.English S.Stage {id} = case id of
  1 -> "Scorch Gorge"
  2 -> "Eeltail Alley"
  3 -> "Hagglefish Market"
  4 -> "Undertow Spillway"
  5 -> "Um'ami Ruins"
  6 -> "Mincemeat Metalworks"
  7 -> "Brinewater Springs"
  8 -> "Barnacle & Dime"
  9 -> "Flounder Heights"
  10 -> "Hammerhead Bridge"
  11 -> "Museum d'Alfonsino"
  12 -> "Mahi-Mahi Resort"
  13 -> "Inkblot Art Academy"
  14 -> "Sturgeon Shipyard"
  15 -> "MakoMart"
  16 -> "Wahoo World"
  17 -> "Humpback Pump Track"
  18 -> "Manta Maria"
  19 -> "Crableg Capital"
  20 -> "Shipshape Cargo Co."
  21 -> "Robo ROM-en"
  22 -> "Bluefin Depot"
  23 -> "Marlin Airport"
  24 -> "Lemuria Hub"
  _ -> ""

showRuleName :: Q.Language -> S.Rule -> String
showRuleName Q.Japanese S.Rule {key} = case key of
  S.SplatZones -> "ガチエリア"
  S.TowerControl -> "ガチヤグラ"
  S.Rainmaker -> "ガチホコバトル"
  S.ClamBlitz -> "ガチアサリ"
  S.TurfWar -> "ナワバリバトル"
showRuleName Q.English S.Rule {key} = case key of
  S.SplatZones -> "Splat Zones"
  S.TowerControl -> "Tower Control"
  S.Rainmaker -> "Rainmaker"
  S.ClamBlitz -> "Clam Blitz"
  S.TurfWar -> "Turf War"

showRuleName' :: Q.Language -> Q.Rule -> String
showRuleName' Q.Japanese Q.TurfWar = "ナワバリバトル"
showRuleName' Q.Japanese Q.SplatZones = "ガチエリア"
showRuleName' Q.Japanese Q.TowerControl = "ガチヤグラ"
showRuleName' Q.Japanese Q.Rainmaker = "ガチホコバトル"
showRuleName' Q.Japanese Q.ClamBlitz = "ガチアサリ"
showRuleName' Q.English Q.TurfWar = "Turf War"
showRuleName' Q.English Q.SplatZones = "Splat Zones"
showRuleName' Q.English Q.TowerControl = "Tower Control"
showRuleName' Q.English Q.Rainmaker = "Rainmaker"
showRuleName' Q.English Q.ClamBlitz = "Clam Blitz"

showMode :: Q.Language -> Q.Mode -> String
showMode Q.Japanese Q.BankaraOpen = "バンカラオープン"
showMode Q.Japanese Q.BankaraChallenge = "バンカラチャレンジ"
showMode Q.Japanese Q.XMatch = "Xマッチ"
showMode Q.Japanese Q.Regular = "レギュラーマッチ"
showMode Q.Japanese Q.Event = "イベントマッチ"
showMode Q.English Q.BankaraOpen = "Anarchy Battle (Open)"
showMode Q.English Q.BankaraChallenge = "Anarchy Battle (Series)"
showMode Q.English Q.XMatch = "X Battle"
showMode Q.English Q.Event = "Challenge"
showMode Q.English Q.Regular = "Regular Battle"

-- 例: 【ガチエリア】バンカラマッチ(オープン) / ユノハナ大渓谷, ゴンズイ地区
showCalendarSummary :: Q.Language -> Q.Mode -> S.Rule -> [S.Stage] -> String
showCalendarSummary language mode rule stages =
  intercalate
    ""
    [ "【" ++ showRuleName language rule ++ "】", -- ex: 【ガチエリア】
      showMode language mode, -- ex: バンカラマッチ(オープン)
      " / ",
      intercalate ", " $ map (showStageName language) stages -- ex: ユノハナ大渓谷, ゴンズイ地区
    ]

showZonedTime :: T.ZonedTime -> String
showZonedTime = T.formatTime T.defaultTimeLocale "%H:%M"

showQueryDescription :: Q.QueryRoot -> String
showQueryDescription Q.QueryRoot {filters, utcOffset, language} = intercalate "\n" ["時差: " ++ showUtcOffset utcOffset, "フィルター: " ++ intercalate "\n" (map showFilter filters)]
  where
    showUtcOffset :: Q.UtcOffsetTimeZone -> String
    showUtcOffset (Q.UtcOffsetTimeZone timeZone) = show timeZone
    showFilter :: Q.FilterCondition -> String
    showFilter Q.FilterCondition {modes, stages, rules, timeSlots} = intercalate ", " ["モード: " ++ showModes modes ++ ", ステージ: " ++ showStages ++ "ルール: " ++ showRules rules ++ ", " ++ intercalate ", 時間帯: " [showTimeSlot timeSlot | timeSlot <- timeSlots]]
    showModes :: Maybe [Q.Mode] -> String
    showModes Nothing = "すべて"
    showModes (Just modes) = intercalate ", " $ map (showMode Q.Japanese) modes
    showRules :: Maybe [Q.Rule] -> String
    showRules Nothing = "すべて"
    showRules (Just rules) = intercalate ", " $ map (showRuleName' language) rules
    showDayOfWeek :: C.DayOfWeek -> String
    showDayOfWeek C.Monday = "月"
    showDayOfWeek C.Tuesday = "火"
    showDayOfWeek C.Wednesday = "水"
    showDayOfWeek C.Thursday = "木"
    showDayOfWeek C.Friday = "金"
    showDayOfWeek C.Saturday = "土"
    showDayOfWeek C.Sunday = "日"
    showDayOfWeeks :: Maybe [Q.TimeSlotDayOfWeek] -> String
    showDayOfWeeks Nothing = "すべての曜日"
    showDayOfWeeks (Just dows) = intercalate ", " $ map (showDayOfWeek . Q.dayOfWeek) dows
    showStages :: Maybe [S.Stage] -> String
    showStages Nothing = "すべてのステージ"
    showStages (Just stages) = intercalate ", " $ map (showStageName Q.Japanese) stages
    showStageFilter :: Q.StageFilter -> String
    showStageFilter Q.StageFilter {matchBothStages, stageIds} = "両方のステージ: " ++ show matchBothStages ++ ", ステージID: " ++ show stageIds
    showTimeSlot :: Q.TimeSlot -> String
    showTimeSlot Q.TimeSlot {start, end, dayOfWeeks} = show start ++ "~" ++ show end ++ "(" ++ showDayOfWeeks dayOfWeeks ++ ")"

-- 例(日本語):
-- 21:00から23:00までガチエリアの予定あります。
-- ・バンカラマッチ
-- ・ステージ: ユノハナ大渓谷, ゴンズイ地区
-- 例(英語):
-- There is a Splat Zones schedule from 21:00 to 23:00.
-- - Bankara Match
-- - Stages: Scorch Gorge, Eeltail Alley
showCalendarDescription :: Q.Language -> Q.Mode -> S.Rule -> [S.Stage] -> (T.ZonedTime, T.ZonedTime) -> String
showCalendarDescription Q.Japanese mode rule stages (startTime, endTime) =
  intercalate
    "\n"
    [ showZonedTime startTime ++ "から" ++ showZonedTime endTime ++ "まで" ++ showRuleName Q.Japanese rule ++ "の予定があります。",
      "・" ++ showMode Q.Japanese mode,
      "・ステージ: " ++ intercalate ", " (map (showStageName Q.Japanese) stages),
      "",
      "スケジュール設定の変更はこちら: https://ink-alarm.pages.dev/ja"
    ]
showCalendarDescription Q.English mode rule stages (startTime, endTime) =
  intercalate
    "\n"
    [ "There is a scheduled " ++ showRuleName Q.English rule ++ " from " ++ showZonedTime startTime ++ " to " ++ showZonedTime endTime ++ ".",
      "- " ++ showMode Q.English mode,
      "- Stages: " ++ intercalate ", " (map (showStageName Q.English) stages),
      "",
      "Click here to change schedule settings: https://ink-alarm.pages.dev/en"
    ]
