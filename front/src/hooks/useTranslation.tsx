// 現状は複雑な翻訳を用意する必要がないため、react-i18next などを使ってません。
// 複雑になってから検討する

import type { FC } from 'react'
import {
  LuCalendarArrowUp,
  LuClipboardCopy,
  LuSquarePlus,
} from 'react-icons/lu'
import { SiGooglecalendar } from 'react-icons/si'
import { useTranslationLanguageContext } from '~/contexts/translationLanguageContext'

export type TranslationKey =
  | 'app.name'
  | 'app.description'
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
  | 'stage.25'
  | 'stage.26'
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
  | 'label.how_to_use'
  | 'label.how_to_use_url'
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
  | 'label.copied'
  | 'label.preview_calendar'
  | 'label.no_schedule'
  | 'label.too_many_schedule'
  | 'label.apply_setting'

const dictionaryJa: Record<TranslationKey, string> = {
  'app.name': 'ガチアラーム',
  'app.description':
    'ガチアラームはスプラトゥーン3のスケジュールをカレンダーアプリに自動転記するためのツールです。',
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
  'stage.25': 'グランドバンカラアリーナ',
  'stage.26': 'デカライン高架下',
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
  'label.how_to_use': '使い方',
  'label.how_to_use_url':
    'https://docs.google.com/presentation/d/1AOBB0h7d-2kclU9cekgac54LM2lPZQ8CJMz7hbwo_-4/edit#slide=id.p',
  'label.match_both_stages': '両方のステージがマッチする場合に限る',
  'label.match_at_least_one_stages': '片方のステージだけがマッチする場合も含む',
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
  'label.copied': 'クリップボードにコピーしました',
  'label.preview_calendar': 'カレンダーのプレビュー',
  'label.no_schedule': '該当するスケジュールはありません',
  'label.too_many_schedule':
    '※該当する予定が多すぎます。絞り込み条件を増やすことをおすすめします。',
  'label.apply_setting': 'カレンダー設定を反映',
}

const dictionaryEn: Record<TranslationKey, string> = {
  'app.name': 'Ink Alarm',
  'app.description':
    'InkAlarm is a tool that automatically transfers the Splatoon 3 schedule to your calendar app.',
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
  'stage.25': 'Grand Splatlands Bowl',
  'stage.26': 'Urchin Underpass',
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
  'label.how_to_use': 'How to use (Japanese)',
  'label.how_to_use_url':
    'https://docs.google.com/presentation/d/1AOBB0h7d-2kclU9cekgac54LM2lPZQ8CJMz7hbwo_-4/edit#slide=id.p',
  'label.match_both_stages': 'Only when both stages matched',
  'label.match_at_least_one_stages': 'Including when one of the stages matched',
  'label.schedule_filter': 'Schedule filter',
  'label.add_schedule_filter': 'Add another condition',
  'label.remove_schedule_filter': 'Remove the above condition',
  'label.rule_filter': 'Rules',
  'label.mode_filter': 'Modes',
  'label.time_filter': 'Time Settings',
  'label.add_timeslot': 'Add a time slot',
  'label.remove_timeslot': 'Remove the time slot',
  'label.stage_filter': 'Stages',
  'label.general_setting': 'General setting',
  'label.language': 'Language',
  'label.time_difference': 'Setting the time difference',
  'label.add_to_calendar_app': 'Add to Calendar App',
  'label.add_to_google_calendar': 'Add to Google Calendar',
  'label.copy_url': 'Copy URL',
  'label.copied': 'Copied to clipboard',
  'label.preview_calendar': 'Preview calendar',
  'label.no_schedule': 'No schedule matched',
  'label.too_many_schedule':
    '* Too many schedules matched. We recommend adding more filter conditions.',
  'label.apply_setting': 'Apply calendar setting',
}

type TranslationComponentKey =
  | 'help.schedule_filter'
  | 'help.time_filter'
  | 'help.language'
  | 'help.time_difference'
  | 'help.add_to_calendar'
  | 'help.preview_calendar'

const componentDictionaryJa: Record<TranslationComponentKey, FC> = {
  'help.schedule_filter': () => (
    <div>
      <h3 className="mb-2 text-lg">スケジュールの表示条件</h3>
      <p>
        下記の<span className="mx-1 bg-nord-6 px-1">ルール</span>,
        <span className="mx-1 bg-nord-6 px-1">モード</span>,
        <span className="mx-1 bg-nord-6 px-1">時間指定</span>,
        <span className="mx-1 bg-nord-6 px-1">ステージ</span>
        で設定した条件すべてに
      </p>
      <p>合致したスケジュールのみ、お使いのカレンダーに同期されます。</p>
      <p className="pt-2">特殊な指定方法として、複数の指定も可能です。</p>
      <p>例:</p>
      <p>
        ①<span className="mx-1 bg-nord-6 px-1">ルール: ガチエリア</span>,
        <span className="mx-1 bg-nord-6 px-1">モード: Xマッチ</span>,
        <span className="mx-1 bg-nord-6 px-1">
          時間指定: 20:00 - 24:00 (月~金)
        </span>
        ,<span className="mx-1 bg-nord-6 px-1">ステージ: すべて</span>
      </p>
      <p>
        ②<span className="mx-1 bg-nord-6 px-1">ルール: すべて</span>,
        <span className="mx-1 bg-nord-6 px-1">モード: イベントマッチ</span>,
        <span className="mx-1 bg-nord-6 px-1">時間指定: すべて</span>,
        <span className="mx-1 bg-nord-6 px-1">ステージ: すべて</span>
      </p>
      <p>のように①②両方の条件を設定すると、</p>
      <p>平日夜のXマッチエリアはすべてカレンダーに登録されつつ、</p>
      <p>
        イベントマッチについては日付等を問わず、すべてのスケジュールが登録されます。
      </p>

      <p className="pt-1">
        複数の条件を指定したい場合は、
        <span className="bg-nord-6 px-1">
          <LuSquarePlus className="inline" /> 別の条件を追加
        </span>
        をクリックしてください。
      </p>
    </div>
  ),
  'help.time_filter': () => (
    <div>
      <h3 className="mb-2 text-lg">時間指定</h3>
      <p>カレンダーに登録されるスケジュールの時間帯と曜日を指定します。</p>
      <p>特殊な指定方法として、以下のような指定が可能です。</p>
      <p className="mt-2 py-1 font-bold">1. 00:00 ~ 00:00</p>
      <p>
        <span className="mr-1 bg-nord-6 px-1">00:00 ~ 00:00</span>
        のように、開始時刻と終了時刻を同じ時刻を設定した場合、
      </p>
      <p>
        <span className="mr-1 bg-nord-6 px-1">
          どの時間帯であってもマッチする
        </span>
        という条件設定になります。
      </p>
      <p className="mt-2 py-1 font-bold">2. 日付またぎ</p>
      <p>
        例:
        <span className="mx-1 bg-nord-6 px-1">20:00 - 02:00, 月曜日</span>
        のように指定した場合
      </p>
      <p>
        ⇒ 月曜日の 20:00 - 23:59 だけではなく、翌日火曜日の 00:00 - 02:00
        の時間帯もカレンダーにスケジュールが登録されます。
      </p>
      <p className="mt-2 py-1 font-bold">3. 複数の時間帯の指定</p>
      <p>下記のように、複数の時間帯を指定することが可能です。</p>
      <p className="mb-2">
        例:
        <span className="mx-1 bg-nord-6 px-1">20:00 - 24:00 (月~金)</span>
        or
        <span className="mx-1 bg-nord-6 px-1">09:00-23:00 (土,日)</span>
      </p>
      <p>
        複数の時間帯を指定したい場合は、
        <span className="bg-nord-6 px-1">
          <LuSquarePlus className="inline" /> 時間指定を追加
        </span>
        をクリックしてください。
      </p>
    </div>
  ),
  'help.language': () => (
    <div>
      <h3 className="mb-2 text-lg">言語</h3>
      <p>
        設定した言語に応じて、カレンダーに登録される言語と設定画面で表示される言語を変更します。
      </p>
    </div>
  ),
  'help.time_difference': () => (
    <div>
      <h3 className="mb-2 text-lg">時差の設定</h3>
      <p>
        こちらの項目は、デフォルト値のままご利用いただくことを推奨いたします。
      </p>
      <p>
        デフォルト設定では、ブラウザの時計をもとにお住いの地域のタイムゾーンを自動的に判別します。
      </p>
      <p>
        表示されているタイムゾーン以外で設定をご希望の場合のみ、設定を変更してください。
      </p>
    </div>
  ),
  'help.add_to_calendar': () => (
    <div>
      <h3 className="mb-2 text-lg">カレンダー設定を反映</h3>
      <p>
        設定した内容で、スプラトゥーンのスケジュールをカレンダーに同期するように設定します。
      </p>
      <p>
        Google カレンダーであれば
        <span className="mx-1 bg-nord-6 px-1">
          <SiGooglecalendar className="inline" /> Google カレンダーに追加
        </span>
        ,
      </p>
      <p>
        iOS 標準のカレンダーアプリ等であれば
        <span className="mx-1 bg-nord-6 px-1">
          <LuCalendarArrowUp className="inline" /> カレンダーアプリに追加
        </span>
        ,
      </p>
      <p>
        その他のカレンダーアプリや、他のボタンでうまく設定反映できない場合は
        <span className="mx-1 bg-nord-6 px-1">
          <LuClipboardCopy className="inline" /> URL をコピー
        </span>
      </p>
      <p>をクリックして、カレンダーアプリに登録してください。</p>
      <p>
        <span className="mx-1 bg-nord-6 px-1">
          [アプリ名] ical ics url インポート
        </span>
        などで検索すると、設定方法が見つかるかもしれません。
      </p>
      <p className="mt-2">
        また、取り込んだスケジュールに対して
        <span className="mx-1 bg-nord-6 px-1">30分前に通知する</span>
        などの設定をすることをおすすめいたします。
      </p>
      <p>
        Google カレンダーでの詳しい設定方法は
        <a
          href="https://docs.google.com/presentation/d/1AOBB0h7d-2kclU9cekgac54LM2lPZQ8CJMz7hbwo_-4/edit#slide=id.g322e2388e0f_0_32"
          target="_blank"
          rel="noreferrer"
          className="mx-1 bg-nord-6 px-1 text-nord-10 italic underline"
        >
          こちら
        </a>
      </p>
      <p className="mt-2">
        ※一部のカレンダーアプリでは、ガチアラームとのスケジュール同期ができない場合があります。
      </p>
    </div>
  ),
  'help.preview_calendar': () => (
    <div>
      <h3 className="mb-2 text-lg">カレンダーのプレビュー</h3>
      <p>実際にカレンダーに登録されるスケジュールを確認することができます。</p>
      <p>
        <span className="mr-1 bg-nord-6 px-1">スケジュールの表示条件</span>,
        <span className="mr-1 bg-nord-6 px-1">一般設定</span>
        で設定した内容に基づいて、
      </p>
      <p>表示される内容は変動します。</p>
      <p className="mt-2">
        スケジュールの件数が多すぎると通知等が煩わしい場合がありますので、
      </p>
      <p>
        適切な件数になるように
        <span className="mr-1 bg-nord-6 px-1">スケジュールの表示条件</span>
        で調整することをお勧めいたします
      </p>
    </div>
  ),
}

const componentDictionaryEn: Record<TranslationComponentKey, FC> = {
  'help.schedule_filter': () => {
    return (
      <div>
        <h3 className="mb-2 text-lg">Schedule filter</h3>
        <p>
          Only schedules that match all the conditions set for
          <span className="mx-1 bg-nord-6 px-1">Rules</span>,
          <span className="mx-1 bg-nord-6 px-1">Modes</span>,
          <span className="mx-1 bg-nord-6 px-1">Time Settings</span>, and
          <span className="mx-1 bg-nord-6 px-1">Stages</span> will be synced to
          your calendar.
        </p>
        <p>Special methods for specifying multiple conditions are available.</p>
        <p>Examples:</p>
        <p>
          ①<span className="mx-1 bg-nord-6 px-1">Rule: Splat Zones</span>,
          <span className="mx-1 bg-nord-6 px-1">Mode: X Match</span>,
          <span className="mx-1 bg-nord-6 px-1">
            Time: 8:00 PM - 12:00 AM (Mon~Fri)
          </span>
          ,<span className="mx-1 bg-nord-6 px-1">Stage: All</span>
        </p>
        <p>
          ②<span className="mx-1 bg-nord-6 px-1">Rule: All</span>,
          <span className="mx-1 bg-nord-6 px-1">Mode: Event Match</span>,
          <span className="mx-1 bg-nord-6 px-1">Time: All</span>,
          <span className="mx-1 bg-nord-6 px-1">Stage: All</span>
        </p>
        <p>
          By setting conditions like ① and ②, schedules for X Match Splat Zones
          during weekday nights
        </p>
        <p>
          will be added to your calendar, while all Event Match schedules are
          added regardless of the date.
        </p>
        <p>
          To specify multiple conditions, click
          <span className="bg-nord-6 px-1">
            <LuSquarePlus className="inline" /> Add Another Condition
          </span>
          .
        </p>
      </div>
    )
  },
  'help.time_filter': () => (
    <div>
      <h3 className="mb-2 text-lg">Time Settings</h3>
      <p>
        Select the time range and days for the schedules to be added to your
        calendar.
      </p>
      <p>Special methods for specifying time include:</p>
      <p className="mt-2 py-1 font-bold">1. 00:00 ~ 00:00</p>
      <p>
        If the start and end times are set to the same, like
        <span className="mr-1 bg-nord-6 px-1">00:00 ~ 00:00</span>, it means
        schedules at any time will match.
      </p>
      <p className="mt-2 py-1 font-bold">2. Spanning Multiple Dates</p>
      <p>
        Example:{' '}
        <span className="mx-1 bg-nord-6 px-1">8:00 PM - 2:00 AM, Monday</span>
        includes Monday 8:00 PM - 11:59 PM and Tuesday 12:00 AM - 2:00 AM.
      </p>
      <p className="mt-2 py-1 font-bold">3. Multiple Time Ranges</p>
      <p>Multiple ranges can be specified, such as:</p>
      <p className="mb-2">
        Example:{' '}
        <span className="mx-1 bg-nord-6 px-1">
          8:00 PM - 12:00 AM (Mon~Fri)
        </span>
        or{' '}
        <span className="mx-1 bg-nord-6 px-1">
          9:00 AM - 11:00 PM (Sat, Sun)
        </span>
        .
      </p>
      <p>
        To specify multiple ranges, click
        <span className="bg-nord-6 px-1">
          <LuSquarePlus className="inline" /> Add a time slot
        </span>
        .
      </p>
    </div>
  ),
  'help.language': () => (
    <div>
      <h3 className="mb-2 text-lg">Language</h3>
      <p>
        Change the language used in the calendar and the settings page based on
        your selection.
      </p>
    </div>
  ),
  'help.time_difference': () => (
    <div>
      <h3 className="mb-2 text-lg">Time Difference Settings</h3>
      <p>We recommend keeping the default settings.</p>
      <p>
        The default setting automatically detects your local timezone based on
        your browser's clock.
      </p>
      <p>
        Change this setting only if you want to use a timezone other than the
        detected one.
      </p>
    </div>
  ),
  'help.add_to_calendar': () => (
    <div>
      <h3 className="mb-2 text-lg">Apply to Calendar</h3>
      <p>Sync Splatoon schedules to your calendar based on your settings.</p>
      <p>
        For Google Calendar, click
        <span className="mx-1 bg-nord-6 px-1">
          <SiGooglecalendar className="inline" /> Add to Google Calendar
        </span>
        .
      </p>
      <p>
        For iOS or other apps, click
        <span className="mx-1 bg-nord-6 px-1">
          <LuCalendarArrowUp className="inline" /> Add to Calendar App
        </span>
        .
      </p>
      <p>
        If neither works, click
        <span className="mx-1 bg-nord-6 px-1">
          <LuClipboardCopy className="inline" /> Copy URL
        </span>
        and add it manually to your app.
      </p>
      <p>
        Search "
        <span className="mx-1 bg-nord-6 px-1">
          [App Name] ical ics url import
        </span>
        " for specific instructions.
      </p>
      <p className="mt-2">
        For Google Calendar setup details, check
        <a
          href="https://docs.google.com/presentation/d/1AOBB0h7d-2kclU9cekgac54LM2lPZQ8CJMz7hbwo_-4/edit#slide=id.g322e2388e0f_0_32"
          target="_blank"
          rel="noreferrer"
          className="mx-1 bg-nord-6 px-1 text-nord-10 italic underline"
        >
          here (Japanese)
        </a>
        .
      </p>
      <p>Note: Some apps may not support syncing schedules with this tool.</p>
    </div>
  ),
  'help.preview_calendar': () => (
    <div>
      <h3 className="mb-2 text-lg">Calendar Preview</h3>
      <p>Preview the schedules that will be added to your calendar.</p>
      <p>
        The content shown changes based on your settings in
        <span className="mr-1 bg-nord-6 px-1">Schedule filter</span>
        and <span className="mr-1 bg-nord-6 px-1">General Settings</span>.
      </p>
      <p className="mt-2">
        If there are too many schedules, adjust the
        <span className="mr-1 bg-nord-6 px-1">Schedule filter</span>
        to reduce clutter.
      </p>
    </div>
  ),
}

type Translate = (key: TranslationKey) => string
type TranslateComponent = (key: TranslationComponentKey) => FC

type UseTranslation = {
  t: Translate
  tc: TranslateComponent
}

export const useTranslation = (): UseTranslation => {
  const { language } = useTranslationLanguageContext()

  const t: Translate = (key: TranslationKey): string => {
    const dictionary = language === 'ja' ? dictionaryJa : dictionaryEn
    return dictionary[key]
  }

  const tc: TranslateComponent = (key: TranslationComponentKey): FC => {
    const dictionary =
      language === 'ja' ? componentDictionaryJa : componentDictionaryEn
    return dictionary[key]
  }

  return { t, tc }
}
