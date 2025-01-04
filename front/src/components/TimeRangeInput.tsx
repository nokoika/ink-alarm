import type { FC } from 'react'
import { FiChevronDown } from 'react-icons/fi'
import type { TimeSlotWithKey } from '~/types/propTypes'
import styles from './TimeRangeInput.module.css'

export const TimeRangeInput: FC<{
  timeSlot: TimeSlotWithKey
  updateStartTime: (key: string, start: string) => void
  updateEndTime: (key: string, end: string) => void
}> = ({ timeSlot, updateStartTime, updateEndTime }) => {
  return (
    <div className="flex items-center gap-2">
      {/* Start Time */}
      <div className="relative">
        <input
          type="time"
          className={`border-0 border-nord-3 border-b bg-transparent pr-6 text-sm focus:border-nord-10 focus:outline-none focus:ring-0 ${styles['calendar-picker-indicator']}`}
          value={timeSlot.start}
          onChange={(e) => updateStartTime(timeSlot.key, e.target.value)}
        />
        <FiChevronDown
          className="pointer-events-none absolute right-0 bottom-1/2 translate-y-1/2 transform text-nord-8"
          size={16}
        />
      </div>
      <span className="">~</span>
      {/* End Time */}
      <div className="relative">
        <input
          type="time"
          className={`border-0 border-nord-3 border-b bg-transparent pr-6 text-sm focus:border-nord-10 focus:outline-none focus:ring-0 ${styles['calendar-picker-indicator']}`}
          value={timeSlot.end}
          onChange={(e) => updateEndTime(timeSlot.key, e.target.value)}
        />
        <FiChevronDown
          className="pointer-events-none absolute right-0 bottom-1/2 translate-y-1/2 transform text-nord-8"
          size={16}
        />
      </div>
    </div>
  )
}
