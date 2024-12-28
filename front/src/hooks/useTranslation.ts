// 現状は複雑な翻訳を用意する必要がないため、react-i18next などを使ってません。
// 複雑になってから検討する

import { useTranslationLanguageContext } from '../contexts/translationLanguageContext.ts'

export type TranslationKey =
  | 'app.name'
  | 'stage.1'
  | 'stage.2'
  | 'stage.3'
  | 'stage.4'
  | 'stage.5'
  | 'stage.6'
  | 'stage.7'
  | 'stage.8'
  | 'stage.9'
  | 'stage.10'
  | 'stage.11'
  | 'stage.12'
  | 'stage.13'
  | 'stage.14'
  | 'stage.15'
  | 'stage.16'
  | 'stage.17'
  | 'stage.18'
  | 'stage.19'
  | 'stage.20'
  | 'stage.21'
  | 'stage.22'
  | 'stage.23'
  | 'stage.24'
  | 'rule.splat_zones'
  | 'rule.tower_control'
  | 'rule.rainmaker'
  | 'rule.clam_blitz'
  | 'rule.turf_war'
  | 'mode.bankara_open'
  | 'mode.bankara_challenge'
  | 'mode.x_match'
  | 'mode.regular'
  | 'mode.event'
  | 'date.monday'
  | 'date.tuesday'
  | 'date.wednesday'
  | 'date.thursday'
  | 'date.friday'
  | 'date.saturday'
  | 'date.sunday'

const dictionaryJa: Record<TranslationKey, string> = {
  'app.name': 'ガチアラーム',
  'stage.1': 'ユノハナ大渓谷',
  'stage.2': 'ゴンズイ地区',
  'stage.3': 'ヤガラ市場',
  'stage.4': 'マテガイ放水路',
  'stage.5': 'ナンプラー遺跡',
  'stage.6': 'ナメロウ金属',
  'stage.7': 'クサヤ温泉',
  'stage.8': 'タラポートショッピングパーク',
  'stage.9': 'ヒラメが丘団地',
  'stage.10': 'マサバ海峡大橋',
  'stage.11': 'キンメダイ美術館',
  'stage.12': 'マヒマヒリゾート＆スパ',
  'stage.13': '海女美術大学',
  'stage.14': 'チョウザメ造船',
  'stage.15': 'ザトウマーケット',
  'stage.16': 'スメーシーワールド',
  'stage.17': 'コンブトラック',
  'stage.18': 'マンタマリア号',
  'stage.19': 'タカアシ経済特区',
  'stage.20': 'オヒョウ海運',
  'stage.21': 'バイガイ亭',
  'stage.22': 'ネギトロ炭鉱',
  'stage.23': 'カジキ空港',
  'stage.24': 'リュウグウターミナル',
  'rule.splat_zones': 'ガチエリア',
  'rule.tower_control': 'ガチヤグラ',
  'rule.rainmaker': 'ガチホコバトル',
  'rule.clam_blitz': 'ガチアサリ',
  'rule.turf_war': 'ナワバリバトル',
  'mode.bankara_open': 'バンカラオープン',
  'mode.bankara_challenge': 'バンカラチャレンジ',
  'mode.x_match': 'Xマッチ',
  'mode.regular': 'レギュラーマッチ',
  'mode.event': 'イベントマッチ',
  'date.monday': '月曜日',
  'date.tuesday': '火曜日',
  'date.wednesday': '水曜日',
  'date.thursday': '木曜日',
  'date.friday': '金曜日',
  'date.saturday': '土曜日',
  'date.sunday': '日曜日',
}

const dictionaryEn: Record<TranslationKey, string> = {
  'app.name': 'Ink Alarm',
  'stage.1': 'Scorch Gorge',
  'stage.2': 'Eeltail Alley',
  'stage.3': 'Hagglefish Market',
  'stage.4': 'Undertow Spillway',
  'stage.5': "Um'ami Ruins",
  'stage.6': 'Mincemeat Metalworks',
  'stage.7': 'Brinewater Springs',
  'stage.8': 'Barnacle & Dime',
  'stage.9': 'Flounder Heights',
  'stage.10': 'Hammerhead Bridge',
  'stage.11': "Museum d'Alfonsino",
  'stage.12': 'Mahi-Mahi Resort',
  'stage.13': 'Inkblot Art Academy',
  'stage.14': 'Sturgeon Shipyard',
  'stage.15': 'MakoMart',
  'stage.16': 'Wahoo World',
  'stage.17': 'Humpback Pump Track',
  'stage.18': 'Manta Maria',
  'stage.19': 'Crableg Capital',
  'stage.20': 'Shipshape Cargo Co.',
  'stage.21': 'Robo ROM-en',
  'stage.22': 'Bluefin Depot',
  'stage.23': 'Marlin Airport',
  'stage.24': 'Lemuria Hub',
  'rule.splat_zones': 'Splat Zones',
  'rule.tower_control': 'Tower Control',
  'rule.rainmaker': 'Rainmaker',
  'rule.clam_blitz': 'Clam Blitz',
  'rule.turf_war': 'Turf War',
  'mode.bankara_open': 'Anarchy Battle (Open)',
  'mode.bankara_challenge': 'Anarchy Battle (Series)',
  'mode.x_match': 'X Battle',
  'mode.regular': 'Regular Battle',
  'mode.event': 'Challenge',
  'date.monday': 'Monday',
  'date.tuesday': 'Tuesday',
  'date.wednesday': 'Wednesday',
  'date.thursday': 'Thursday',
  'date.friday': 'Friday',
  'date.saturday': 'Saturday',
  'date.sunday': 'Sunday',
}

type Translate = (key: TranslationKey) => string

type UseTranslation = { t: Translate }

export const useTranslation = (): UseTranslation => {
  const { language } = useTranslationLanguageContext()

  const t: Translate = (key: TranslationKey): string => {
    const dictionary = language === 'ja' ? dictionaryJa : dictionaryEn
    return dictionary[key]
  }

  return { t }
}
