import type { FilterCondition, TimeSlot } from '~/types/querySchema'

// lint/suspicious/noArrayIndexKey 対策
export type TimeSlotWithKey = Required<TimeSlot> & { key: string }
export type FilterConditionWithKey = Required<FilterCondition> & {
  key: string
} & {
  timeSlots: TimeSlotWithKey[]
}
