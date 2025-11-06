import pako from 'pako'
import type {
  FilterCondition,
  Query,
  StageFilter,
  TimeSlot,
} from '~/types/querySchema'
import { DayOfWeek, Language, Mode, Rule } from '~/types/querySchema'

const languageValues = new Set(Object.values(Language))
const modeValues = new Set(Object.values(Mode))
const ruleValues = new Set(Object.values(Rule))
const dayOfWeekValues = new Set(Object.values(DayOfWeek))

const normalizeBase64Url = (value: string): string => {
  const base64 = value.replace(/-/g, '+').replace(/_/g, '/')
  const paddingLength = (4 - (base64.length % 4)) % 4
  return base64 + '='.repeat(paddingLength)
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

const isStageFilter = (value: unknown): value is StageFilter => {
  if (!value || typeof value !== 'object') {
    return false
  }
  const stageFilter = value as StageFilter
  return (
    typeof stageFilter.matchBothStages === 'boolean' &&
    Array.isArray(stageFilter.stageIds) &&
    stageFilter.stageIds.every((stageId) => typeof stageId === 'number')
  )
}

const isTimeSlot = (value: unknown): value is TimeSlot => {
  if (!value || typeof value !== 'object') {
    return false
  }
  const timeSlot = value as TimeSlot
  return (
    typeof timeSlot.start === 'string' &&
    typeof timeSlot.end === 'string' &&
    (timeSlot.dayOfWeeks === undefined ||
      (Array.isArray(timeSlot.dayOfWeeks) &&
        timeSlot.dayOfWeeks.every((day) =>
          dayOfWeekValues.has(day as DayOfWeek),
        )))
  )
}

const isFilterCondition = (value: unknown): value is FilterCondition => {
  if (!value || typeof value !== 'object') {
    return false
  }
  const filter = value as FilterCondition
  const modesValid =
    filter.modes === undefined ||
    (Array.isArray(filter.modes) &&
      filter.modes.every((mode) => modeValues.has(mode as Mode)))
  const rulesValid =
    filter.rules === undefined ||
    (Array.isArray(filter.rules) &&
      filter.rules.every((rule) => ruleValues.has(rule as Rule)))
  const stagesValid =
    filter.stages === undefined || isStageFilter(filter.stages)
  const timeSlotsValid =
    filter.timeSlots === undefined ||
    (Array.isArray(filter.timeSlots) &&
      filter.timeSlots.every((timeSlot) => isTimeSlot(timeSlot)))

  return modesValid && rulesValid && stagesValid && timeSlotsValid
}

const isQuery = (value: unknown): value is Query => {
  if (!value || typeof value !== 'object') {
    return false
  }
  const query = value as Query
  return (
    languageValues.has(query.language as Language) &&
    (query.utcOffset === undefined || typeof query.utcOffset === 'string') &&
    Array.isArray(query.filters) &&
    query.filters.every((filter) => isFilterCondition(filter))
  )
}

export const decodeCalendarQuery = (search: string): Query | null => {
  const params = new URLSearchParams(search)
  const encodedQuery = params.get('query')
  if (!encodedQuery) {
    return null
  }

  try {
    const bytes = decodeBase64Url(encodedQuery)
    const json = toJsonString(bytes)
    const parsed = JSON.parse(json)
    if (isQuery(parsed)) {
      return parsed
    }
    return null
  } catch (_error) {
    return null
  }
}
