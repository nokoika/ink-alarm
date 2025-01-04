import type { Query } from '~/types/querySchema'

export const generateIcalUrl = (
  query: Query,
): {
  https: string
  webcal: string
  googleCalendar: string
} => {
  const json = JSON.stringify(query)
  const base64 = btoa(json)
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
