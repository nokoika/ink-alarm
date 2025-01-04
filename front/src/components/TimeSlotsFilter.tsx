import type { FC } from 'react'
import { LuSquareMinus, LuSquarePlus } from 'react-icons/lu'
import { useTranslation } from '~/hooks/useTranslation'
import type { TimeSlotWithKey } from '~/types/propTypes'
import { DayOfWeek } from '~/types/querySchema'
import { generateDefaultTimeSlot } from '~/utils/generateInitialState'
import { CheckboxList } from './CheckboxList'
import { IconButton } from './IconButton'
import { TimeRangeInput } from './TimeRangeInput'

export const TimeSlotsFilter: FC<{
  timeSlots: TimeSlotWithKey[]
  updateTimeSlots: (timeSlots: TimeSlotWithKey[]) => void
}> = ({ timeSlots, updateTimeSlots }) => {
  const { t } = useTranslation()

  const addTimeSlotAfter = (key: string) => {
    const index = timeSlots.findIndex((timeSlot) => timeSlot.key === key)
    const newTimeSlot = generateDefaultTimeSlot()
    updateTimeSlots([
      ...timeSlots.slice(0, index + 1),
      newTimeSlot,
      ...timeSlots.slice(index + 1),
    ])
  }

  const removeTimeSlot = (key: string) => {
    updateTimeSlots(timeSlots.filter((timeSlot) => timeSlot.key !== key))
  }

  const updateDayOfWeek = (
    timeSlotKey: string,
    dayOfWeek: DayOfWeek,
    enable: boolean,
  ) => {
    updateTimeSlots(
      timeSlots.map((timeSlot) =>
        timeSlot.key === timeSlotKey
          ? {
              ...timeSlot,
              dayOfWeeks: enable
                ? [...timeSlot.dayOfWeeks, dayOfWeek]
                : timeSlot.dayOfWeeks.filter((d) => d !== dayOfWeek),
            }
          : timeSlot,
      ),
    )
  }

  const updateStartTime = (timeSlotKey: string, start: string) => {
    updateTimeSlots(
      timeSlots.map((timeSlot) =>
        timeSlot.key === timeSlotKey ? { ...timeSlot, start } : timeSlot,
      ),
    )
  }

  const updateEndTime = (timeSlotKey: string, end: string) => {
    updateTimeSlots(
      timeSlots.map((timeSlot) =>
        timeSlot.key === timeSlotKey ? { ...timeSlot, end } : timeSlot,
      ),
    )
  }

  const timeSlotsWithContents = timeSlots.map((timeSlot) => ({
    timeSlot,
    contents: [
      {
        key: DayOfWeek.mon,
        text: t('date.monday'),
        enabled: timeSlot.dayOfWeeks.includes(DayOfWeek.mon),
      },
      {
        key: DayOfWeek.tue,
        text: t('date.tuesday'),
        enabled: timeSlot.dayOfWeeks.includes(DayOfWeek.tue),
      },
      {
        key: DayOfWeek.wed,
        text: t('date.wednesday'),
        enabled: timeSlot.dayOfWeeks.includes(DayOfWeek.wed),
      },
      {
        key: DayOfWeek.thu,
        text: t('date.thursday'),
        enabled: timeSlot.dayOfWeeks.includes(DayOfWeek.thu),
      },
      {
        key: DayOfWeek.fri,
        text: t('date.friday'),
        enabled: timeSlot.dayOfWeeks.includes(DayOfWeek.fri),
      },
      {
        key: DayOfWeek.sat,
        text: t('date.saturday'),
        enabled: timeSlot.dayOfWeeks.includes(DayOfWeek.sat),
      },
      {
        key: DayOfWeek.sun,
        text: t('date.sunday'),
        enabled: timeSlot.dayOfWeeks.includes(DayOfWeek.sun),
      },
    ],
  }))

  return (
    <div className="space-y-10">
      {timeSlotsWithContents.map(({ timeSlot, contents }) => {
        return (
          <div key={timeSlot.key} className="space-y-4">
            <div className="flex gap-4 flex-col md:flex-row">
              <TimeRangeInput
                timeSlot={timeSlot}
                updateStartTime={updateStartTime}
                updateEndTime={updateEndTime}
              />
              <IconButton
                icon={LuSquarePlus}
                text={t('label.add_timeslot')}
                onClick={() => addTimeSlotAfter(timeSlot.key)}
              />
              {timeSlots.length >= 2 && (
                <IconButton
                  icon={LuSquareMinus}
                  text={t('label.remove_timeslot')}
                  onClick={() => removeTimeSlot(timeSlot.key)}
                />
              )}
            </div>
            <CheckboxList
              contents={contents}
              updateItem={(dayOfWeek, enable) =>
                updateDayOfWeek(timeSlot.key, dayOfWeek, enable)
              }
              className="grid gap-x-2 grid-cols-2 md:grid-cols-5"
            />
          </div>
        )
      })}
    </div>
  )
}
