import type { FC } from 'react'
import { FiChevronDown } from 'react-icons/fi'
import type { TimeSlotWithKey } from '~/types/propTypes'

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
          className="bg-transparent border-0 border-b border-nord-3 text-sm focus:outline-none focus:ring-0 focus:border-nord-10 pr-6"
          value={timeSlot.start}
          onChange={(e) => updateStartTime(timeSlot.key, e.target.value)}
        />
        <FiChevronDown
          className="absolute right-0 bottom-1/2 transform translate-y-1/2 text-nord-8 pointer-events-none"
          size={16}
        />
      </div>
      <span className="">~</span>
      {/* End Time */}
      <div className="relative">
        <input
          type="time"
          className="bg-transparent border-0 border-b border-nord-3 text-sm focus:outline-none focus:ring-0 focus:border-nord-10 pr-6"
          value={timeSlot.end}
          onChange={(e) => updateEndTime(timeSlot.key, e.target.value)}
        />
        <FiChevronDown
          className="absolute right-0 bottom-1/2 transform translate-y-1/2 text-nord-8 pointer-events-none"
          size={16}
        />
      </div>
    </div>
  )
}
