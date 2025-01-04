// 現状は複雑な翻訳を用意する必要がないため、react-i18next などを使ってません。
// 複雑になってから検討する

import { useTranslationLanguageContext } from '~/contexts/translationLanguageContext'

export type TranslationKey =
  | 'app.name'
  | 'language.ja'
  | 'language.en'
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
  | 'label.match_both_stages'
  | 'label.match_at_least_one_stages'
  | 'label.time_difference'
  | 'label.schedule_filter'
  | 'label.add_schedule_filter'
  | 'label.remove_schedule_filter'
  | 'label.rule_filter'
  | 'label.mode_filter'
  | 'label.time_filter'
  | 'label.add_timeslot'
  | 'label.remove_timeslot'
  | 'label.stage_filter'
  | 'label.general_setting'
  | 'label.language'
  | 'label.add_to_calendar_app'
  | 'label.add_to_google_calendar'
  | 'label.copy_url'
  | 'label.preview_calendar'
  | 'label.no_schedule'
  | 'label.too_many_schedule'
  | 'label.apply_setting'

const dictionaryJa: Record<TranslationKey, string> = {
  'app.name': 'ガチアラーム',
  'language.ja': '日本語',
  'language.en': 'English',
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
  'label.match_both_stages': '両方のステージがマッチする場合に限る',
  'label.match_at_least_one_stages': '片方のステージがマッチする場合も含む',
  'label.schedule_filter': 'スケジュールの表示条件',
  'label.add_schedule_filter': '別の条件を追加',
  'label.remove_schedule_filter': '上記条件を削除',
  'label.rule_filter': 'ルール',
  'label.mode_filter': 'モード',
  'label.time_filter': '時間指定',
  'label.add_timeslot': '時間指定を追加',
  'label.remove_timeslot': '時間指定の削除',
  'label.stage_filter': 'ステージ',
  'label.general_setting': '一般設定',
  'label.language': '言語',
  'label.time_difference': '時差の設定',
  'label.add_to_calendar_app': 'カレンダーアプリに追加',
  'label.add_to_google_calendar': 'Googleカレンダーに追加',
  'label.copy_url': 'URLをコピー',
  'label.preview_calendar': 'カレンダーのプレビュー',
  'label.no_schedule': '該当するスケジュールはありません',
  'label.too_many_schedule':
    '※該当する予定が多すぎます。絞り込み条件を増やすことをおすすめします。',
  'label.apply_setting': 'カレンダー設定を反映',
}

const dictionaryEn: Record<TranslationKey, string> = {
  'app.name': 'Ink Alarm',
  'language.ja': '日本語',
  'language.en': 'English',
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
  'label.match_both_stages': 'Only when both stages matched',
  'label.match_at_least_one_stages': 'Including when one of the stages matched',
  'label.schedule_filter': 'Schedule filter',
  'label.add_schedule_filter': 'Add another condition',
  'label.remove_schedule_filter': 'Remove the above condition',
  'label.rule_filter': 'Rule',
  'label.mode_filter': 'Mode',
  'label.time_filter': 'Time',
  'label.add_timeslot': 'Add a time slot',
  'label.remove_timeslot': 'Remove the time slot',
  'label.stage_filter': 'Stage',
  'label.general_setting': 'General setting',
  'label.language': 'Language',
  'label.time_difference': 'Setting the time difference',
  'label.add_to_calendar_app': 'Add to calendar app',
  'label.add_to_google_calendar': 'Add to Google Calendar',
  'label.copy_url': 'Copy URL',
  'label.preview_calendar': 'Preview calendar',
  'label.no_schedule': 'No schedule matched',
  'label.too_many_schedule':
    '* Too many schedules matched. We recommend adding more filter conditions.',
  'label.apply_setting': 'Apply calendar setting',
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
