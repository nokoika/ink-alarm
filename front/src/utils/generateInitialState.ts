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

export const generateInitilalUtcOffset = (): string => {
  return format(new Date(), 'xxx')
}
