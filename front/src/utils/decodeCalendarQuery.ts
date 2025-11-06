import type { Query } from '~/types/querySchema'
import { decodeBase64UrlToJson } from '~/utils/calendarQueryCodec'

export const decodeCalendarQuery = (search: string): Query | null => {
  const params = new URLSearchParams(search)
  const encodedQuery = params.get('query')
  if (!encodedQuery) {
    return null
  }

  try {
    const json = decodeBase64UrlToJson(encodedQuery)
    return JSON.parse(json) as Query
  } catch (_error) {
    return null
  }
}
