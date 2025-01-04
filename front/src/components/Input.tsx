import { type FC, useState } from 'react'
import { FiCalendar, FiSun } from 'react-icons/fi'
import {
  LuCalendarArrowUp,
  LuClipboardCopy,
  LuEarth,
  LuLanguages,
  LuMountainSnow,
  LuSquareMinus,
  LuSquarePlus,
  LuStar,
} from 'react-icons/lu'
import { PiFootballBold, PiGearBold } from 'react-icons/pi'
import { RiImageAiLine, RiTimerFlashLine } from 'react-icons/ri'
import { SiGooglecalendar } from 'react-icons/si'
import { useTranslationLanguageContext } from '~/contexts/translationLanguageContext'
import { useCalendar } from '~/hooks/useCalendar'
import { useFilterCondition } from '~/hooks/useFilterCondition'
import { useTranslation } from '~/hooks/useTranslation'
import { generateIcalUrl } from '~/utils/generateIcalUrl'
import { generateInitilalUtcOffset } from '~/utils/generateInitialState'
import { EventList } from './EventList'
import { IconButton } from './IconButton'
import { InputBlock } from './InputBlock'
import { ModesFilter } from './ModesFilter'
import { RulesFilter } from './RulesFilter'
import { StagesFilter } from './StagesFilter'
import { SwitchLanguage } from './SwitchLanguage'
import { TimeSlotsFilter } from './TimeSlotsFilter'
import { UtcOffset } from './UtcOffset'

export const Input: FC = () => {
  const { language, setLanguage } = useTranslationLanguageContext()
  const [utcOffset, setUtcOffset] = useState<string>(
    generateInitilalUtcOffset(),
  )
  const { t } = useTranslation()
  const { filters, addFilterAfter, updateFilter, removeFilter } =
    useFilterCondition()

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
        <div className="grid gap-4 md:grid-cols-2">
          <InputBlock title={t('label.language')} icon={LuLanguages}>
            <SwitchLanguage language={language} setLanguage={setLanguage} />
          </InputBlock>
          <InputBlock title={t('label.time_difference')} icon={LuEarth}>
            <UtcOffset utcOffset={utcOffset} setUtcOffset={setUtcOffset} />
          </InputBlock>
        </div>
      </InputBlock>
      <InputBlock title={t('label.apply_setting')} icon={LuStar}>
        <div className="flex justify-center gap-4 flex-col md:flex-row">
          <IconButton
            icon={SiGooglecalendar}
            text={t('label.add_to_google_calendar')}
            buttonColorClass="bg-nord-12"
            onClick={() => {
              window.open(icalUrls.googleCalendar, '_blank')
            }}
          />
          <IconButton
            icon={LuCalendarArrowUp}
            text={t('label.add_to_calendar_app')}
            buttonColorClass="bg-nord-15"
            onClick={() => {
              window.open(icalUrls.webcal, '_blank')
            }}
          />
          <IconButton
            icon={LuClipboardCopy}
            text={t('label.copy_url')}
            onClick={() => {
              navigator.clipboard.writeText(icalUrls.https)
              // TODO: toast などに変更
              alert(t('label.copied'))
            }}
          />
        </div>
      </InputBlock>
      <InputBlock title={t('label.preview_calendar')} icon={RiImageAiLine}>
        {events.length >= 10 && (
          <p className="text-nord-12 mb-2">{t('label.too_many_schedule')}</p>
        )}
        <EventList events={events} utcOffset={utcOffset} />
      </InputBlock>
    </div>
  )
}