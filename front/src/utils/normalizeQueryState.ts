import { v4 as uuidv4 } from 'uuid'
import type { FilterConditionWithKey, TimeSlotWithKey } from '~/types/propTypes'
import type {
  FilterCondition,
  Language,
  Query,
  StageFilter,
  TimeSlot,
} from '~/types/querySchema'
import { Mode, Rule } from '~/types/querySchema'
import {
  generateDefaultFilter,
  generateDefaultTimeSlot,
} from '~/utils/generateInitialState'

export type NormalizedQueryState = {
  filters: FilterConditionWithKey[]
  utcOffset?: string
  language: Language
}

const allModes = Object.values(Mode)
const allRules = Object.values(Rule)

const createDefaultStageFilter = (): StageFilter => {
  const { stages } = generateDefaultFilter()
  return {
    matchBothStages: stages.matchBothStages,
    stageIds: [...stages.stageIds],
  }
}

const normalizeTimeSlot = (timeSlot?: TimeSlot): TimeSlotWithKey => {
  const fallback = generateDefaultTimeSlot()
  const dayOfWeeks =
    timeSlot?.dayOfWeeks && timeSlot.dayOfWeeks.length > 0
      ? Array.from(new Set(timeSlot.dayOfWeeks))
      : [...fallback.dayOfWeeks]

  return {
    key: uuidv4(),
    start: timeSlot?.start ?? fallback.start,
    end: timeSlot?.end ?? fallback.end,
    dayOfWeeks,
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

  const stages = filter.stages
    ? {
        matchBothStages: filter.stages.matchBothStages,
        stageIds: Array.from(new Set(filter.stages.stageIds)),
      }
    : createDefaultStageFilter()

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

export const normalizeQueryState = (query: Query): NormalizedQueryState => {
  const filters: FilterConditionWithKey[] =
    query.filters.length === 0
      ? [generateDefaultFilter()]
      : query.filters.map((filter) => normalizeFilter(filter))

  const normalizedState: NormalizedQueryState = {
    filters,
    language: query.language,
  }

  if (query.utcOffset !== undefined) {
    normalizedState.utcOffset = query.utcOffset
  }

  return normalizedState
}
