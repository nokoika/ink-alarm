import ICAL from 'ical.js'
import { useCallback, useEffect, useState } from 'react'
import type { Event } from '~/components/EventList'

export const useCalendar = (calendarUrl: string): Event[] => {
  const [events, setEvents] = useState<Event[]>([])

  const fetchIcalEvents = useCallback(async (): Promise<Event[]> => {
    const response = await fetch(calendarUrl)
    const icalData = await response.text()
    const jcalData = ICAL.parse(icalData)
    const comp = new ICAL.Component(jcalData)
    const vevents = comp.getAllSubcomponents('vevent')

    return vevents.map((vevent) => {
      const event = new ICAL.Event(vevent)
      return {
        id: event.uid,
        title: event.summary,
        start: event.startDate.toJSDate(),
        end: event.endDate.toJSDate(),
      }
    })
  }, [calendarUrl])

  useEffect(() => {
    fetchIcalEvents().then((events) => setEvents(events))
  }, [fetchIcalEvents])

  return events
}
