import { format } from 'date-fns'
import { v4 as uuidv4 } from 'uuid'
import type { FilterConditionWithKey, TimeSlotWithKey } from '~/types/propTypes'
import type { FilterCondition, TimeSlot } from '~/types/querySchema'
import { DayOfWeek, Mode, type Query, Rule } from '~/types/querySchema'

// すべての選択肢をONにした初期値を生成
export const generateDefaultFilter = (): FilterConditionWithKey => ({
  key: uuidv4(),
  modes: [Mode.x],
  stages: {
    matchBothStages: false,
    stageIds: [
      // グランドバンカラアリーナ以外すべて
      1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21,
      22, 23, 24, 26,
    ],
  },
  rules: [Rule.area],
  timeSlots: [generateDefaultTimeSlot()],
})

export const generateDefaultTimeSlot = (): TimeSlotWithKey => ({
  key: uuidv4(),
  start: '00:00',
  end: '00:00',
  dayOfWeeks: [
    DayOfWeek.mon,
    DayOfWeek.tue,
    DayOfWeek.wed,
    DayOfWeek.thu,
    DayOfWeek.fri,
    DayOfWeek.sat,
    DayOfWeek.sun,
  ],
})

export const generateInitilalUtcOffset = (
  restoredQuery: Query | null,
): string => {
  return restoredQuery?.utcOffset ?? format(new Date(), 'xxx')
}

const createTimeSlotWithKey = (timeSlot: TimeSlot): TimeSlotWithKey => {
  const fallback = generateDefaultTimeSlot()
  return {
    key: fallback.key,
    start: timeSlot.start ?? fallback.start,
    end: timeSlot.end ?? fallback.end,
    dayOfWeeks: timeSlot.dayOfWeeks ?? fallback.dayOfWeeks,
  }
}

const createFilterWithKey = (
  filter: FilterCondition,
): FilterConditionWithKey => {
  const fallback = generateDefaultFilter()
  return {
    ...fallback,
    modes: filter.modes ?? fallback.modes,
    rules: filter.rules ?? fallback.rules,
    stages: filter.stages ?? fallback.stages,
    timeSlots:
      filter.timeSlots?.map((timeSlot) => createTimeSlotWithKey(timeSlot)) ??
      fallback.timeSlots,
  }
}

export const generateInitialFilters = (
  restoredQuery: Query | null,
): FilterConditionWithKey[] => {
  if (!restoredQuery) {
    return [generateDefaultFilter()]
  }
  return restoredQuery.filters.map((filter) => createFilterWithKey(filter))
}
