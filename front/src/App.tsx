import { format } from 'date-fns'
import { type FC, useState } from 'react'
import TimezoneSelect, {
  type ITimezone,
  type ITimezoneOption,
} from 'react-timezone-select'
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
  Rule,
  type StageFilter,
  type TimeSlot,
} from '~/types/querySchema'

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
    <div className="flex gap-2">
      {contents.map(({ key, text, enabled }) => (
        <label key={key} className="relative cursor-pointer">
          <input
            type="checkbox"
            className="sr-only peer"
            checked={enabled}
            onChange={() => {
              if (enabled) {
                removeRule(key)
              } else {
                addRule(key)
              }
            }}
          />
          <span className="block px-4 py-2 text-gray-700 bg-gray-200 rounded peer-checked:bg-blue-500 peer-checked:text-white transition-colors duration-200 hover:bg-gray-300">
            {text}
          </span>
        </label>
      ))}
    </div>
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
    <div className="flex gap-2">
      {contents.map(({ key, text, enabled }) => (
        <label key={key} className="relative cursor-pointer">
          <input
            type="checkbox"
            className="sr-only peer"
            checked={enabled}
            onChange={() => {
              if (enabled) {
                removeMode(key)
              } else {
                addMode(key)
              }
            }}
          />
          <span className="block px-4 py-2 text-gray-700 bg-gray-200 rounded peer-checked:bg-blue-500 peer-checked:text-white transition-colors duration-200 hover:bg-gray-300">
            {text}
          </span>
        </label>
      ))}
    </div>
  )
}

const TimeSlotFilter: FC<{
  timeSlot: TimeSlotWithKey
  updateTimeSlot: (newTimeSlot: TimeSlotWithKey) => void
  removeTimeSlot?: (() => void) | undefined
}> = ({ timeSlot, updateTimeSlot, removeTimeSlot }) => {
  const { t } = useTranslation()
  const contents = [
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
  ]

  const updateDayOfWeek = (dayOfWeek: DayOfWeek, enable: boolean) => {
    updateTimeSlot({
      ...timeSlot,
      dayOfWeeks: enable
        ? [...timeSlot.dayOfWeeks, dayOfWeek]
        : timeSlot.dayOfWeeks.filter((d) => d !== dayOfWeek),
    })
  }
  const updateStartTime = (start: string) => {
    updateTimeSlot({ ...timeSlot, start })
  }
  const updateEndTime = (end: string) => {
    updateTimeSlot({ ...timeSlot, end })
  }

  return (
    <div className="flex gap-4">
      <div className="flex items-center gap-2">
        <input
          type="time"
          className="px-3 py-2 border border-gray-300 rounded focus:outline-none focus:border-blue-500 focus:ring-1 focus:ring-blue-500"
          value={timeSlot.start}
          onChange={(e) => updateStartTime(e.target.value)}
        />
        <span className="text-gray-700">~</span>
        <input
          type="time"
          className="px-3 py-2 border border-gray-300 rounded focus:outline-none focus:border-blue-500 focus:ring-1 focus:ring-blue-500"
          value={timeSlot.end}
          onChange={(e) => updateEndTime(e.target.value)}
        />
      </div>
      <div className="flex flex-wrap gap-2">
        {contents.map(({ key, text, enabled }) => (
          <label key={key} className="cursor-pointer">
            <input
              type="checkbox"
              className="sr-only peer"
              checked={enabled}
              onChange={() => updateDayOfWeek(key, !enabled)}
            />
            <span className="block px-3 py-2 text-gray-700 bg-gray-200 rounded peer-checked:bg-blue-500 peer-checked:text-white hover:bg-gray-100 transition-colors">
              {text}
            </span>
          </label>
        ))}
      </div>

      {removeTimeSlot && (
        <button
          type="button"
          className="px-4 py-2 text-white bg-blue-500 rounded hover:bg-blue-600 transition-colors"
          onClick={removeTimeSlot}
        >
          -
        </button>
      )}
    </div>
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
    <div className="grid gap-2">
      <div className="grid gap-2 [grid-template-columns:repeat(auto-fit,minmax(210px,1fr))]">
        {contents.map(({ key, text, enabled }) => (
          <label key={key} className="relative cursor-pointer">
            <input
              type="checkbox"
              className="sr-only peer"
              checked={enabled}
              onChange={() => {
                if (enabled) {
                  removeStage(key)
                } else {
                  addStage(key)
                }
              }}
            />
            <span className="block px-3 py-2 text-sm text-center text-gray-700 bg-gray-200 rounded peer-checked:bg-blue-500 peer-checked:text-white hover:bg-gray-300 transition-colors">
              {text}
            </span>
          </label>
        ))}
      </div>
      <label className="cursor-pointer">
        <input
          type="checkbox"
          className="sr-only peer"
          checked={stages.matchBothStages}
          onChange={toggleMatchBothStages}
        />
        <span className="block px-3 py-2 text-sm text-center text-gray-700 bg-gray-200 rounded peer-checked:bg-blue-500 peer-checked:text-white hover:bg-gray-300 transition-colors">
          {t('label.match_both_stages')}
        </span>
      </label>
    </div>
  )
}

const TimeSlotsFilter: FC<{
  timeSlots: TimeSlotWithKey[]
  updateTimeSlots: (timeSlots: TimeSlotWithKey[]) => void
}> = ({ timeSlots, updateTimeSlots }) => {
  const addTimeSlot = () => {
    updateTimeSlots([...timeSlots, generateDefaultTimeSlot()])
  }

  const removeTimeSlot = (key: string) => {
    updateTimeSlots(timeSlots.filter((timeSlot) => timeSlot.key !== key))
  }

  const updateTimeSlot = (newTimeSlot: TimeSlotWithKey) => {
    updateTimeSlots(
      timeSlots.map((timeSlot) =>
        timeSlot.key === newTimeSlot.key
          ? { ...timeSlot, ...newTimeSlot }
          : timeSlot,
      ),
    )
  }

  return (
    <div>
      <button type="button" onClick={addTimeSlot}>
        時間指定の追加
      </button>
      <div className="space-y-2">
        {timeSlots.map((timeSlot) => (
          <TimeSlotFilter
            key={timeSlot.key}
            timeSlot={timeSlot}
            updateTimeSlot={updateTimeSlot}
            removeTimeSlot={
              timeSlots.length >= 2
                ? () => removeTimeSlot(timeSlot.key)
                : undefined
            }
          />
        ))}
      </div>
    </div>
  )
}

const AddFilter: FC<{ onClick: (...args: unknown[]) => unknown }> = ({
  onClick,
}) => {
  return (
    <div className="flex justify-center">
      <button
        type="button"
        className="px-4 py-2 text-white bg-blue-500 rounded hover:bg-blue-600 transition-colors"
        onClick={onClick}
      >
        +
      </button>
    </div>
  )
}

const RemoveFilter: FC<{ onClick: (...args: unknown[]) => unknown }> = ({
  onClick,
}) => {
  return (
    <div className="flex justify-center">
      <button
        type="button"
        className="px-4 py-2 text-white bg-blue-500 rounded hover:bg-blue-600 transition-colors"
        onClick={onClick}
      >
        -
      </button>
    </div>
  )
}

const FilterWidget: FC<{
  filter: FilterConditionWithKey
  updateFilter: (filter: Partial<FilterConditionWithKey>) => void
  removeFilter?: (() => void) | undefined
}> = ({ filter, updateFilter, removeFilter }) => {
  return (
    <div className="space-y-2">
      <RulesFilter
        rules={filter.rules}
        updateRules={(rules) => updateFilter({ rules })}
      />
      <ModesFilter
        modes={filter.modes}
        updateModes={(modes) => updateFilter({ modes })}
      />
      <TimeSlotsFilter
        timeSlots={filter.timeSlots}
        updateTimeSlots={(timeSlots) => updateFilter({ timeSlots })}
      />
      <StagesFilter
        stages={filter.stages}
        updateStages={(stages) => updateFilter({ stages })}
      />
      {removeFilter && <RemoveFilter onClick={removeFilter} />}
    </div>
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
    <div>
      <input
        type="radio"
        id="ja"
        name="language"
        value="ja"
        checked={language === Language.ja}
        onChange={() => setLanguage(Language.ja)}
      />
      <label htmlFor="ja">{t('language.ja')}</label>
      <input
        type="radio"
        id="en"
        name="language"
        value="en"
        checked={language === Language.en}
        onChange={() => setLanguage(Language.en)}
      />
      <label htmlFor="en">{t('language.en')}</label>
    </div>
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
  const [showTimezoneInput, setShowTimezoneInput] = useState(false)

  const [tz, setTz] = useState<ITimezone>(
    // デフォルト値はブラウザのタイムゾーン
    Intl.DateTimeFormat().resolvedOptions().timeZone,
  )

  const handleChange = (newTz: ITimezoneOption) => {
    setTz(newTz)
    if (newTz.offset) {
      setUtcOffset(convertOffsetToString(newTz.offset))
    }
  }

  return (
    <div>
      {showTimezoneInput ? (
        <TimezoneSelect value={tz} onChange={handleChange} />
      ) : (
        <button type="button" onClick={() => setShowTimezoneInput(true)}>
          タイムゾーン設定変更 ({utcOffset})
        </button>
      )}
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

const Input: FC = () => {
  const [filters, setFilters] = useState<FilterConditionWithKey[]>([
    generateDefaultFilter(),
  ])
  const [utcOffset, setUtcOffset] = useState<string>(getInitilalUtcOffset())
  const { language, setLanguage } = useTranslationLanguageContext()

  const addFilter = () => {
    setFilters((prev) => [...prev, generateDefaultFilter()])
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

  return (
    <div>
      <AddFilter onClick={addFilter} />
      <SwitchLanguage language={language} setLanguage={setLanguage} />
      <UtcOffset utcOffset={utcOffset} setUtcOffset={setUtcOffset} />
      {filters.map((filter) => (
        <FilterWidget
          key={filter.key}
          filter={filter}
          updateFilter={(newFilter) => updateFilter(filter.key, newFilter)}
          removeFilter={
            filters.length >= 2 ? () => removeFilter(filter.key) : undefined
          }
        />
      ))}
    </div>
  )
}

export const App: FC = () => {
  return (
    <TranslationLanguageProvider>
      <Input />
    </TranslationLanguageProvider>
  )
}
