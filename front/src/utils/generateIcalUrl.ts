import pako from 'pako'
import type { Query } from '~/types/querySchema'

// 全選択な条件は省略して送信しても同じであるため、URL文字数制限の都合で詰める
const createOptimizedQuery = (query: Query): Query => {
  return {
    language: query.language,
    ...(query.utcOffset && { utcOffset: query.utcOffset }),
    filters: query.filters.map((filter) => {
      // 雑かも。暫定対応なので許して

      // モードがバンカラオープン、バンカラチャレンジ、X、レギュラー、イベントマッチぜんぶ選択されている場合は省略
      const modes = filter.modes?.length === 5 ? undefined : filter.modes

      // ルールがナワバリ、エリア、ヤグラ、ホコ、アサリぜんぶ選択されている場合は省略
      const rules = filter.rules?.length === 5 ? undefined : filter.rules

      const filteredTimeSlots = filter.timeSlots?.filter((timeSlot) => {
        // 曜日がぜんぶ選択されていない場合は省略しない
        if (timeSlot.dayOfWeeks?.length !== 7) {
          return true
        }
        // 開始時刻と終了時刻が 00:00 じゃない場合は省略しない
        if (timeSlot.start !== '00:00' || timeSlot.end !== '00:00') {
          return true
        }
        // 全条件が無意味な場合は省略
        return false
      })

      const timeSlots =
        filteredTimeSlots?.length === 0 ? undefined : filteredTimeSlots

      // ステージがぜんぶ選択されている場合は省略
      const stages =
        filter.stages?.stageIds.length === 24 ? undefined : filter.stages

      return {
        ...(modes && { modes }),
        ...(rules && { rules }),
        ...(timeSlots && { timeSlots }),
        ...(stages && { stages }),
      }
    }),
  }
}

export const generateIcalUrl = (
  query: Query,
): {
  https: string
  webcal: string
  googleCalendar: string
} => {
  const json = JSON.stringify(createOptimizedQuery(query))
  const gzip = pako.gzip(json)
  const base64 = btoa(String.fromCharCode(...gzip))
  const base64url = base64
    // biome-ignore lint/performance/useTopLevelRegex: 遅くてもいいです
    .replace(/=+$/, '')
    .replace(/\+/g, '-')
    .replace(/\//g, '_')

  return {
    https: `${import.meta.env.VITE_API_URL}?query=${base64url}`,
    webcal: `${import.meta.env.VITE_WEBCAL_URL}?query=${base64url}`,
    googleCalendar: `https://calendar.google.com/calendar/u/0/r?cid=${decodeURI(`${import.meta.env.VITE_WEBCAL_URL}?query=${base64url}`)}`,
  }
}
