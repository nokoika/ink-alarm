import { format } from 'date-fns'
import { ja } from 'date-fns/locale'
import ICAL from 'ical.js'

import {
  type ChangeEvent,
  type FC,
  type ReactNode,
  useCallback,
  useEffect,
  useState,
} from 'react'
import type { IconType } from 'react-icons'
import {
  FiCalendar,
  FiCheckSquare,
  FiChevronDown,
  FiSquare,
  FiSun,
} from 'react-icons/fi'
import {
  LuCalendarArrowUp,
  LuClipboardCopy,
  LuEarth,
  LuLanguages,
  LuMountainSnow,
  LuSquareMinus,
  LuSquarePlus,
} from 'react-icons/lu'
import { PiFootballBold, PiGearBold } from 'react-icons/pi'
import { RiImageAiLine, RiTimerFlashLine } from 'react-icons/ri'
import { SiGooglecalendar } from 'react-icons/si'
import { useTimezoneSelect } from 'react-timezone-select'
import { v4 as uuidv4 } from 'uuid'
import {
  TranslationLanguageProvider,
  useTranslationLanguageContext,
} from '~/contexts/translationLanguageContext'
import { type TranslationKey, useTranslation } from '~/hooks/useTranslation'
import {
  DayOfWeek,
  type FilterCondition,
  Language,
  Mode,
  type Query,
  Rule,
  type StageFilter,
  type TimeSlot,
} from '~/types/querySchema'

const InputBlock: FC<{
  title: string
  icon: IconType
  children: ReactNode
}> = ({ title, icon: Icon, children }) => {
  return (
    <div className="grid gap-4 border-4 px-4 py-3 border-nord-1">
      <div className="flex items-center gap-2">
        <div className="text-nord-10">
          <Icon />
        </div>
        <h2 className="text-lg font-semibold text-nord-6">{title}</h2>
        <div className="text-nord-10">
          <Icon />
        </div>
      </div>
      <div>{children}</div>
    </div>
  )
}

// tsx 文法の中では、arrow だと generics が使えないため function で書く
function CheckboxList<T extends string | number>({
  contents,
  updateItem,
  radio = false,
}: {
  contents: { key: T; text: string; enabled: boolean }[]
  updateItem: (key: T, enabled: boolean) => void
  radio?: boolean
}) {
  return (
    <div className="grid gap-2">
      <div className="grid gap-2 grid-cols-3">
        {contents.map(({ key, text, enabled }) => (
          <label key={key} className="flex items-center cursor-pointer">
            <input
              type={radio ? 'radio' : 'checkbox'}
              className="hidden peer"
              checked={enabled}
              onChange={() => {
                updateItem(key, !enabled)
              }}
            />
            <span className="flex items-center justify-center w-5 h-5 text-nord-3 peer-checked:text-nord-8 transition-colors">
              {enabled ? <FiCheckSquare size={20} /> : <FiSquare size={20} />}
            </span>
            <span className="ml-2 text-sm ">{text}</span>
          </label>
        ))}
      </div>
    </div>
  )
}

const RulesFilter: FC<{
  rules: Rule[]
  updateRules: (rules: Rule[]) => void
}> = ({ rules, updateRules }) => {
  const { t } = useTranslation()

  const contents: { key: Rule; text: string; enabled: boolean }[] = [
    {
      key: Rule.nawabari,
      text: t('rule.turf_war'),
      enabled: rules.includes(Rule.nawabari),
    },
    {
      key: Rule.area,
      text: t('rule.splat_zones'),
      enabled: rules.includes(Rule.area),
    },
    {
      key: Rule.yagura,
      text: t('rule.tower_control'),
      enabled: rules.includes(Rule.yagura),
    },
    {
      key: Rule.hoko,
      text: t('rule.rainmaker'),
      enabled: rules.includes(Rule.hoko),
    },
    {
      key: Rule.asari,
      text: t('rule.clam_blitz'),
      enabled: rules.includes(Rule.asari),
    },
  ]

  const addRule = (rule: Rule) => {
    updateRules([...rules, rule])
  }
  const removeRule = (rule: Rule) => {
    updateRules(rules.filter((r) => r !== rule))
  }

  return (
    <CheckboxList
      contents={contents}
      updateItem={(key, enabled) => {
        if (enabled) {
          addRule(key)
        } else {
          removeRule(key)
        }
      }}
    />
  )
}

const ModesFilter: FC<{
  modes: Mode[]
  updateModes: (modes: Mode[]) => void
}> = ({ modes, updateModes }) => {
  const { t } = useTranslation()

  const contents: { key: Mode; text: string; enabled: boolean }[] = [
    {
      key: Mode.x,
      text: t('mode.x_match'),
      enabled: modes.includes(Mode.x),
    },
    {
      key: Mode.event,
      text: t('mode.event'),
      enabled: modes.includes(Mode.event),
    },
    {
      key: Mode.bankara_open,
      text: t('mode.bankara_open'),
      enabled: modes.includes(Mode.bankara_open),
    },
    {
      key: Mode.bankara_challenge,
      text: t('mode.bankara_challenge'),
      enabled: modes.includes(Mode.bankara_challenge),
    },
    {
      key: Mode.regular,
      text: t('mode.regular'),
      enabled: modes.includes(Mode.regular),
    },
  ]

  const addMode = (mode: Mode) => {
    updateModes([...modes, mode])
  }
  const removeMode = (mode: Mode) => {
    updateModes(modes.filter((m) => m !== mode))
  }

  return (
    <CheckboxList
      contents={contents}
      updateItem={(key, enabled) => {
        if (enabled) {
          addMode(key)
        } else {
          removeMode(key)
        }
      }}
    />
  )
}

const StagesFilter: FC<{
  stages: StageFilter
  updateStages: (sf: StageFilter) => void
}> = ({ stages, updateStages }) => {
  const { t } = useTranslation()
  const contents = Array.from(
    { length: 24 }, // 全ステージ数
    (_, index) => ({
      key: index + 1, // 固定長であるため index で OK
      text: t(`stage.${index + 1}` as TranslationKey),
      enabled: stages.stageIds.includes(index + 1),
    }),
  )

  const addStage = (stageId: number) => {
    updateStages({
      ...stages,
      stageIds: [...stages.stageIds, stageId],
    })
  }
  const removeStage = (stageId: number) => {
    updateStages({
      ...stages,
      stageIds: stages.stageIds.filter((id) => id !== stageId),
    })
  }
  const toggleMatchBothStages = () => {
    updateStages({
      ...stages,
      matchBothStages: !stages.matchBothStages,
    })
  }

  return (
    <div className="space-y-4">
      <CheckboxList
        contents={contents}
        updateItem={(key, enabled) => {
          if (enabled) {
            addStage(key)
          } else {
            removeStage(key)
          }
        }}
      />
      <CheckboxList
        contents={[
          {
            key: 'matchBothStages',
            text: t('label.match_both_stages'),
            enabled: stages.matchBothStages,
          },
          {
            key: 'matchAtLeastOneStages',
            text: t('label.match_at_least_one_stages'),
            enabled: !stages.matchBothStages,
          },
        ]}
        updateItem={() => toggleMatchBothStages()}
        radio={true}
      />
    </div>
  )
}

const TimeRangeInput: FC<{
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

const TimeSlotsFilter: FC<{
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
    <div>
      <div className="space-y-2">
        {timeSlotsWithContents.map(({ timeSlot, contents }) => {
          return (
            <div key={timeSlot.key} className="space-y-4">
              <div className="flex gap-4">
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
              />
            </div>
          )
        })}
      </div>
    </div>
  )
}

const IconButton: FC<{
  onClick: (...args: unknown[]) => unknown
  icon: IconType
  text: string
}> = ({ onClick, icon: Icon, text }) => {
  return (
    <button
      type="button"
      onClick={onClick}
      className="px-3 py-2 text-white bg-nord-3 rounded-sm hover:bg-nord-14 transition-colors"
    >
      <div className="flex items-center gap-2">
        <Icon className="block" />
        <p className="text-xs">{text}</p>
      </div>
    </button>
  )
}

// lint/suspicious/noArrayIndexKey 対策
type TimeSlotWithKey = Required<TimeSlot> & { key: string }
type FilterConditionWithKey = Required<FilterCondition> & { key: string } & {
  timeSlots: TimeSlotWithKey[]
}

const SwitchLanguage: FC<{
  language: Language
  setLanguage: (language: Language) => void
}> = ({ language, setLanguage }) => {
  const { t } = useTranslation()
  return (
    <CheckboxList
      contents={[
        {
          key: Language.ja,
          text: t('language.ja'),
          enabled: language === Language.ja,
        },
        {
          key: Language.en,
          text: t('language.en'),
          enabled: language === Language.en,
        },
      ]}
      updateItem={(key, enabled) => {
        if (enabled) {
          setLanguage(key)
        }
      }}
      radio={true}
    />
  )
}

const convertOffsetToString = (offsetNumber: number) => {
  // 符号判定
  const sign = offsetNumber < 0 ? '-' : '+'

  // 絶対値を取得
  const abs = Math.abs(offsetNumber)

  // 時・分に分解 (3.5h => 3h + 0.5h(=30min))
  const hours = Math.floor(abs)
  const minutes = Math.round((abs - hours) * 60)

  // 2桁のゼロ埋め文字列に
  const hh = String(hours).padStart(2, '0')
  const mm = String(minutes).padStart(2, '0')

  return `${sign}${hh}:${mm}`
}

const UtcOffset: FC<{
  utcOffset: string
  setUtcOffset: (offset: string) => void
}> = ({ utcOffset, setUtcOffset }) => {
  const [timezone, setTimezone] = useState<string>(
    Intl.DateTimeFormat().resolvedOptions().timeZone,
  )

  const { options, parseTimezone } = useTimezoneSelect({
    displayValue: 'UTC',
    labelStyle: 'offsetHidden',
  })

  const selected: string | undefined = options.find(
    (option) =>
      option.offset &&
      convertOffsetToString(option.offset) === utcOffset &&
      option.value === timezone,
  )?.value

  const onChange = (e: ChangeEvent<HTMLSelectElement>) => {
    const parsed = parseTimezone(e.target.value)
    setTimezone(parsed.value)
    if (parsed.offset) {
      setUtcOffset(convertOffsetToString(parsed.offset))
    }
  }

  return (
    <div className="flex items-center gap-4">
      <span className="text-sm font-medium">UTC {utcOffset}</span>
      <div className="relative">
        <select
          onChange={onChange}
          value={selected ?? 'none'}
          className="appearance-none bg-transparent border-0 border-b border-nord-3 text-sm focus:outline-none focus:ring-0 focus:border-nord-10 pr-6 max-w-64"
        >
          <option
            value="none"
            disabled={true}
            className="text-sm bg-nord-2 p-1 appearance-none"
          >
            -
          </option>
          {options.map(
            (option) =>
              !!option.offset && (
                <option
                  key={option.value}
                  value={option.value}
                  className="text-sm bg-nord-2 p-1 appearance-none"
                >
                  {option.label}
                </option>
              ),
          )}
        </select>
        <FiChevronDown
          className="absolute right-0 bottom-1/2 transform translate-y-1/2 text-nord-8 pointer-events-none"
          size={16}
        />
      </div>
    </div>
  )
}

const getInitilalUtcOffset = (): string => {
  return format(new Date(), 'xxx')
}

// すべての選択肢をONにした初期値を生成
const generateDefaultFilter = (): FilterConditionWithKey => ({
  key: uuidv4(),
  modes: [
    Mode.x,
    Mode.event,
    Mode.bankara_open,
    Mode.bankara_challenge,
    Mode.regular,
  ],
  stages: {
    matchBothStages: false,
    stageIds: Array.from({ length: 24 }, (_, index) => index + 1),
  },
  rules: [Rule.area, Rule.asari, Rule.hoko, Rule.nawabari, Rule.yagura],
  timeSlots: [generateDefaultTimeSlot()],
})

const generateDefaultTimeSlot = (): TimeSlotWithKey => ({
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

const generateIcalUrl = (
  query: Query,
): {
  https: string
  webcal: string
  googleCalendar: string
} => {
  const json = JSON.stringify(query)
  const base64 = btoa(json)
  const base64url = base64
    // biome-ignore lint/performance/useTopLevelRegex: 遅くてもいいです
    .replace(/=+$/, '')
    .replace(/\+/g, '-')
    .replace(/\//g, '_')

  return {
    https: `${import.meta.env.VITE_API_URL}?query=${base64url}`,
    webcal: `${import.meta.env.VITE_WEBCAL_URL}?query=${base64url}`,
    googleCalendar: `https://calendar.google.com/calendar/u/0/r?cid=${decodeURI(`${import.meta.env.VITE_WEBCAL_URL}?query=${base64url}`)}`,
  }
}

type Event = {
  title: string
  start: Date
  end: Date
}

const EventList: FC<{ events: Event[] }> = ({ events }) => {
  const { language } = useTranslationLanguageContext()
  const { t } = useTranslation()
  return (
    <div>
      {events.length === 0 ? (
        <p className="text-nord-5">{t('label.no_schedule')}</p>
      ) : (
        <div className="grid grid-cols-3 gap-2">
          {events.map((event) => (
            <div
              key={JSON.stringify(event)}
              className="px-3 py-2 bg-nord-1 transition"
            >
              <div className="text-sm font-semibold text-nord-9">
                {event.title}
              </div>
              <p className="text-nord-5 text-xs">
                {language === Language.ja && (
                  <>
                    {format(event.start, 'M/d (EEE) H:mm', { locale: ja })}~
                    {format(event.end, 'M/d (EEE) H:mm', { locale: ja })}
                  </>
                )}
                {language === Language.en && (
                  <>
                    {format(event.start, 'EEEE, MMMM d, H:mm')} ~{' '}
                    {format(event.end, 'EEEE, MMMM d, H:mm')}
                  </>
                )}
              </p>
            </div>
          ))}
        </div>
      )}
    </div>
  )
}

const useCalendar = (calendarUrl: string): Event[] => {
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

const Input: FC = () => {
  const [filters, setFilters] = useState<FilterConditionWithKey[]>([
    generateDefaultFilter(),
  ])
  const [utcOffset, setUtcOffset] = useState<string>(getInitilalUtcOffset())
  const { language, setLanguage } = useTranslationLanguageContext()
  const { t } = useTranslation()

  // 指定した要素の直後にフィルターを追加
  const addFilterAfter = (key: string) => {
    setFilters((prev) => {
      const index = prev.findIndex((f) => f.key === key)
      const newFilter = generateDefaultFilter()
      return [...prev.slice(0, index + 1), newFilter, ...prev.slice(index + 1)]
    })
  }

  const updateFilter = (
    key: string,
    newFilter: Partial<FilterConditionWithKey>,
  ) => {
    setFilters((prev) =>
      prev.map((f) => (f.key === key ? { ...f, ...newFilter } : f)),
    )
  }

  const removeFilter = (key: string) => {
    setFilters((prev) => prev.filter((f) => f.key !== key))
  }

  const icalUrls = generateIcalUrl({ filters, utcOffset, language })
  const events = useCalendar(icalUrls.https)

  return (
    <div className="max-w-5xl mx-auto space-y-4 p-2">
      <InputBlock title={t('label.schedule_filter')} icon={FiCalendar}>
        <div className="grid gap-10">
          {filters.map((filter) => (
            <div key={filter.key} className="space-y-2">
              <InputBlock title={t('label.rule_filter')} icon={PiFootballBold}>
                <RulesFilter
                  rules={filter.rules}
                  updateRules={(rules) => updateFilter(filter.key, { rules })}
                />
              </InputBlock>
              <InputBlock title={t('label.mode_filter')} icon={FiSun}>
                <ModesFilter
                  modes={filter.modes}
                  updateModes={(modes) => updateFilter(filter.key, { modes })}
                />
              </InputBlock>
              <InputBlock
                title={t('label.time_filter')}
                icon={RiTimerFlashLine}
              >
                <TimeSlotsFilter
                  timeSlots={filter.timeSlots}
                  updateTimeSlots={(timeSlots) =>
                    updateFilter(filter.key, { timeSlots })
                  }
                />
              </InputBlock>
              <InputBlock title={t('label.stage_filter')} icon={LuMountainSnow}>
                <StagesFilter
                  stages={filter.stages}
                  updateStages={(stages) =>
                    updateFilter(filter.key, { stages })
                  }
                />
              </InputBlock>

              <div className="flex justify-center gap-4">
                <IconButton
                  onClick={() => addFilterAfter(filter.key)}
                  icon={LuSquarePlus}
                  text={t('label.add_schedule_filter')}
                />
                {filters.length >= 2 && (
                  <IconButton
                    onClick={() => removeFilter(filter.key)}
                    icon={LuSquareMinus}
                    text={t('label.remove_schedule_filter')}
                  />
                )}
              </div>
            </div>
          ))}
        </div>
      </InputBlock>
      <InputBlock title={t('label.general_setting')} icon={PiGearBold}>
        <div className="grid grid-cols-2 gap-4">
          <InputBlock title={t('label.language')} icon={LuLanguages}>
            <SwitchLanguage language={language} setLanguage={setLanguage} />
          </InputBlock>
          <InputBlock title={t('label.time_difference')} icon={LuEarth}>
            <UtcOffset utcOffset={utcOffset} setUtcOffset={setUtcOffset} />
          </InputBlock>
        </div>
      </InputBlock>
      <div className="flex justify-center gap-4">
        <IconButton
          icon={SiGooglecalendar}
          text={t('label.add_to_google_calendar')}
          onClick={() => {
            window.open(icalUrls.googleCalendar, '_blank')
          }}
        />
        <IconButton
          icon={LuCalendarArrowUp}
          text={t('label.add_to_calendar_app')}
          onClick={() => {
            window.open(icalUrls.webcal, '_blank')
          }}
        />
        <IconButton
          icon={LuClipboardCopy}
          text={t('label.copy_url')}
          onClick={() => {
            navigator.clipboard.writeText(icalUrls.https)
          }}
        />
      </div>
      <InputBlock title={t('label.preview_calendar')} icon={RiImageAiLine}>
        {events.length >= 10 && (
          <p className="text-nord-12 mb-2">{t('label.too_many_schedule')}</p>
        )}
        <EventList events={events} />
      </InputBlock>
    </div>
  )
}

export const App: FC = () => {
  return (
    <TranslationLanguageProvider>
      <div className="bg-nord-0 min-h-screen text-nord-4">
        <Input />
      </div>
    </TranslationLanguageProvider>
  )
}
