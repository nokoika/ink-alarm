import pako from 'pako'
import { v4 as uuidv4 } from 'uuid'
import type { FilterConditionWithKey, TimeSlotWithKey } from '~/types/propTypes'
import type {
  FilterCondition,
  Query,
  StageFilter,
  TimeSlot,
} from '~/types/querySchema'
import { type Language, Mode, Rule } from '~/types/querySchema'
import {
  generateDefaultFilter,
  generateDefaultTimeSlot,
} from '~/utils/generateInitialState'

const allModes = Object.values(Mode)
const allRules = Object.values(Rule)

const defaultFilterTemplate = generateDefaultFilter()
const defaultStageFilter = {
  matchBothStages: defaultFilterTemplate.stages.matchBothStages,
  stageIds: [...defaultFilterTemplate.stages.stageIds],
}
const defaultTimeSlotTemplate = generateDefaultTimeSlot()
const defaultStageIdsLength = defaultStageFilter.stageIds.length

const BASE64_PADDING_REGEX = /=+$/g
const BASE64_URL_DASH_REGEX = /-/g
const BASE64_URL_UNDERSCORE_REGEX = /_/g
const BASE64_PLUS_REGEX = /\+/g
const BASE64_SLASH_REGEX = /\//g

const createOptimizedQuery = (query: Query): Query => {
  return {
    language: query.language,
    ...(query.utcOffset && { utcOffset: query.utcOffset }),
    filters: query.filters.map((filter) => {
      const modes =
        filter.modes?.length === allModes.length ? undefined : filter.modes
      const rules =
        filter.rules?.length === allRules.length ? undefined : filter.rules

      const filteredTimeSlots = filter.timeSlots?.filter((timeSlot) => {
        if (timeSlot.dayOfWeeks?.length !== 7) {
          return true
        }
        if (timeSlot.start !== '00:00' || timeSlot.end !== '00:00') {
          return true
        }
        return false
      })

      const timeSlots =
        filteredTimeSlots && filteredTimeSlots.length > 0
          ? filteredTimeSlots
          : undefined

      const stages =
        filter.stages?.stageIds.length === defaultStageIdsLength
          ? undefined
          : filter.stages

      return {
        ...(modes && { modes }),
        ...(rules && { rules }),
        ...(timeSlots && { timeSlots }),
        ...(stages && { stages }),
      }
    }),
  }
}

const normalizeTimeSlot = (timeSlot?: TimeSlot): TimeSlotWithKey => {
  const dayOfWeeks =
    timeSlot?.dayOfWeeks && timeSlot.dayOfWeeks.length > 0
      ? Array.from(new Set(timeSlot.dayOfWeeks))
      : [...defaultTimeSlotTemplate.dayOfWeeks]

  return {
    key: uuidv4(),
    start: timeSlot?.start ?? defaultTimeSlotTemplate.start,
    end: timeSlot?.end ?? defaultTimeSlotTemplate.end,
    dayOfWeeks,
  }
}

const normalizeStageFilter = (stageFilter?: StageFilter) => {
  if (!stageFilter) {
    return {
      matchBothStages: defaultStageFilter.matchBothStages,
      stageIds: [...defaultStageFilter.stageIds],
    }
  }

  return {
    matchBothStages: stageFilter.matchBothStages,
    stageIds: Array.from(new Set(stageFilter.stageIds)),
  }
}

const normalizeFilter = (filter: FilterCondition): FilterConditionWithKey => {
  const modes =
    filter.modes && filter.modes.length > 0
      ? Array.from(new Set(filter.modes))
      : [...allModes]

  const rules =
    filter.rules && filter.rules.length > 0
      ? Array.from(new Set(filter.rules))
      : [...allRules]

  const stages = normalizeStageFilter(filter.stages)

  const timeSlots =
    filter.timeSlots && filter.timeSlots.length > 0
      ? filter.timeSlots.map((timeSlot) => normalizeTimeSlot(timeSlot))
      : [normalizeTimeSlot()]

  return {
    key: uuidv4(),
    modes,
    rules,
    stages,
    timeSlots,
  }
}

const normalizeBase64Url = (value: string): string => {
  const paddingLength = (4 - (value.length % 4)) % 4
  return (
    value
      .replace(BASE64_URL_DASH_REGEX, '+')
      .replace(BASE64_URL_UNDERSCORE_REGEX, '/') + '='.repeat(paddingLength)
  )
}

const decodeBase64Url = (value: string): Uint8Array => {
  const normalized = normalizeBase64Url(value)
  const binary = atob(normalized)
  const bytes = new Uint8Array(binary.length)
  for (let index = 0; index < binary.length; index += 1) {
    bytes[index] = binary.charCodeAt(index)
  }
  return bytes
}

const toJsonString = (bytes: Uint8Array): string => {
  try {
    return pako.ungzip(bytes, { to: 'string' }) as string
  } catch (gzipError) {
    try {
      return new TextDecoder().decode(bytes)
    } catch {
      throw gzipError
    }
  }
}

export type RestoredQueryState = {
  filters: FilterConditionWithKey[]
  language: Language
  utcOffset?: string
}

export const decodeCalendarQuery = (
  search: string,
): RestoredQueryState | null => {
  const params = new URLSearchParams(search)
  const encodedQuery = params.get('query')
  if (!encodedQuery) {
    return null
  }

  try {
    const bytes = decodeBase64Url(encodedQuery)
    const json = toJsonString(bytes)
    const parsed = JSON.parse(json) as Partial<Query>

    if (
      !parsed ||
      typeof parsed !== 'object' ||
      !parsed.language ||
      !parsed.filters
    ) {
      return null
    }

    const filters: FilterConditionWithKey[] =
      parsed.filters.length > 0
        ? parsed.filters.map((filter) => normalizeFilter(filter))
        : [generateDefaultFilter()]

    const restored: RestoredQueryState = {
      filters,
      language: parsed.language,
    }

    if (typeof parsed.utcOffset === 'string' && parsed.utcOffset.length > 0) {
      restored.utcOffset = parsed.utcOffset
    }

    return restored
  } catch (_error) {
    return null
  }
}

const encodeQueryToBase64Url = (query: Query): string => {
  const json = JSON.stringify(createOptimizedQuery(query))
  const gzip = pako.gzip(json)
  const base64 = btoa(String.fromCharCode(...gzip))
  return base64
    .replace(BASE64_PADDING_REGEX, '')
    .replace(BASE64_PLUS_REGEX, '-')
    .replace(BASE64_SLASH_REGEX, '_')
}

export const generateIcalUrl = (
  query: Query,
): {
  https: string
  webcal: string
  googleCalendar: string
} => {
  const base64url = encodeQueryToBase64Url(query)

  return {
    https: `${import.meta.env.VITE_API_URL}?query=${base64url}`,
    webcal: `${import.meta.env.VITE_WEBCAL_URL}?query=${base64url}`,
    googleCalendar: `https://calendar.google.com/calendar/u/0/r?cid=${decodeURI(`${import.meta.env.VITE_WEBCAL_URL}?query=${base64url}`)}`,
  }
}

export const encodeCalendarQuery = encodeQueryToBase64Url
