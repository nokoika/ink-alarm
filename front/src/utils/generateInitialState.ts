import { format } from 'date-fns'
import { v4 as uuidv4 } from 'uuid'
import type { FilterConditionWithKey, TimeSlotWithKey } from '~/types/propTypes'
import { DayOfWeek, Mode, Rule } from '~/types/querySchema'

// すべての選択肢をONにした初期値を生成
export const generateDefaultFilter = (): FilterConditionWithKey => ({
  key: uuidv4(),
  modes: [Mode.x],
  stages: {
    matchBothStages: false,
    stageIds: Array.from({ length: 24 }, (_, index) => index + 1),
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

export const generateInitilalUtcOffset = (): string => {
  return format(new Date(), 'xxx')
}
