module Translation (showCalendarSummary, showCalendarDescription) where

import Data.List (intercalate)
import qualified Data.Time as T
import qualified Query as Q
import qualified SplaApi as S
import Prelude (String, map, ($), (++))

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

showMatchType :: Q.Language -> Q.MatchType -> String
showMatchType Q.Japanese Q.BankaraOpen = "バンカラマッチ(オープン)"
showMatchType Q.Japanese Q.BankaraChallenge = "バンカラマッチ(チャレンジ)"
showMatchType Q.Japanese Q.XMatch = "Xマッチ"
showMatchType Q.Japanese Q.Regular = "レギュラーマッチ"
showMatchType Q.Japanese Q.Event = "イベントマッチ"
showMatchType Q.English Q.BankaraOpen = "Anarchy Battle (Open)"
showMatchType Q.English Q.BankaraChallenge = "Anarchy Battle (Series)"
showMatchType Q.English Q.XMatch = "X Battle"
showMatchType Q.English Q.Event = "Challenge"
showMatchType Q.English Q.Regular = "Regular Battle"

-- 例: 【ガチエリア】バンカラマッチ(オープン) / ユノハナ大渓谷, ゴンズイ地区
showCalendarSummary :: Q.Language -> Q.MatchType -> S.Rule -> [S.Stage] -> String
showCalendarSummary language matchType rule stages =
  intercalate ""
    [ "【" ++ showRuleName language rule ++ "】", -- ex: 【ガチエリア】
      showMatchType language matchType, -- ex: バンカラマッチ(オープン)
      " / ",
      intercalate ", " $ map (showStageName language) stages -- ex: ユノハナ大渓谷, ゴンズイ地区
    ]

showZonedTime :: T.ZonedTime -> String
showZonedTime = T.formatTime T.defaultTimeLocale "%H:%M"

-- 例(日本語):
-- 21:00から23:00までガチエリアの予定あります。
-- ・バンカラマッチ
-- ・ステージ: ユノハナ大渓谷, ゴンズイ地区
-- 例(英語):
-- There is a Splat Zones schedule from 21:00 to 23:00.
-- - Bankara Match
-- - Stages: Scorch Gorge, Eeltail Alley
showCalendarDescription :: Q.Language -> Q.MatchType -> S.Rule -> [S.Stage] -> (T.ZonedTime, T.ZonedTime) -> String
showCalendarDescription Q.Japanese matchType rule stages (startTime, endTime) =
  intercalate
    "\n"
    [ showZonedTime startTime ++ "から" ++ showZonedTime endTime ++ "まで" ++ showRuleName Q.Japanese rule ++ "の予定があります。",
      "・" ++ showMatchType Q.Japanese matchType,
      "・ステージ: " ++ intercalate ", " (map (showStageName Q.Japanese) stages)
    ]
showCalendarDescription Q.English matchType rule stages (startTime, endTime) =
  intercalate
    "\n"
    [ "There is a scheduled " ++ showRuleName Q.English rule ++ " from " ++ showZonedTime startTime ++ " to " ++ showZonedTime endTime ++ ".",
      "- " ++ showMatchType Q.English matchType,
      "- Stages: " ++ intercalate ", " (map (showStageName Q.English) stages)
    ]
