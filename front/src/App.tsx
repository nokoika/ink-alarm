import type { FC } from 'react'
import { TranslationLanguageProvider } from '~/contexts/translationLanguageContext'
import { type TranslationKey, useTranslation } from '~/hooks/useTranslation'

const RulesFilter: FC = () => {
  const { t } = useTranslation()
  return (
    <div className="flex gap-2">
      {[
        t('rule.splat_zones'),
        t('rule.tower_control'),
        t('rule.rainmaker'),
        t('rule.clam_blitz'),
      ].map((option) => (
        <label key={option} className="relative cursor-pointer">
          <input type="checkbox" className="sr-only peer" />
          <span className="block px-4 py-2 text-gray-700 bg-gray-200 rounded peer-checked:bg-blue-500 peer-checked:text-white transition-colors duration-200 hover:bg-gray-300">
            {option}
          </span>
        </label>
      ))}
    </div>
  )
}

const ModesFilter: FC = () => {
  const { t } = useTranslation()
  return (
    <div className="flex gap-2">
      {[
        t('mode.x_match'),
        t('mode.event'),
        t('mode.bankara_open'),
        t('mode.bankara_challenge'),
        t('mode.regular'),
      ].map((option) => (
        <label key={option} className="relative cursor-pointer">
          <input type="checkbox" className="sr-only peer" />
          <span className="block px-4 py-2 text-gray-700 bg-gray-200 rounded peer-checked:bg-blue-500 peer-checked:text-white transition-colors duration-200 hover:bg-gray-300">
            {option}
          </span>
        </label>
      ))}
    </div>
  )
}

const TimeSlotFilter: FC = () => {
  const { t } = useTranslation()
  const daysOfWeek = [
    {
      key: 'mon',
      text: t('date.monday'),
    },
    {
      key: 'tue',
      text: t('date.tuesday'),
    },
    {
      key: 'wed',
      text: t('date.wednesday'),
    },
    {
      key: 'thu',
      text: t('date.thursday'),
    },
    {
      key: 'fri',
      text: t('date.friday'),
    },
    {
      key: 'sat',
      text: t('date.saturday'),
    },
    {
      key: 'sun',
      text: t('date.sunday'),
    },
  ]
  return (
    <div className="flex gap-4">
      <div className="flex items-center gap-2">
        <input
          type="time"
          className="px-3 py-2 border border-gray-300 rounded focus:outline-none focus:border-blue-500 focus:ring-1 focus:ring-blue-500"
          defaultValue="19:00"
        />
        <span className="text-gray-700">~</span>
        <input
          type="time"
          className="px-3 py-2 border border-gray-300 rounded focus:outline-none focus:border-blue-500 focus:ring-1 focus:ring-blue-500"
          defaultValue="00:00"
        />
      </div>
      <div className="flex flex-wrap gap-2">
        {daysOfWeek.map(({ key, text }) => (
          <label key={key} className="cursor-pointer">
            <input type="checkbox" className="sr-only peer" />
            <span className="block px-3 py-2 text-gray-700 bg-gray-200 rounded peer-checked:bg-blue-500 peer-checked:text-white hover:bg-gray-100 transition-colors">
              {text}
            </span>
          </label>
        ))}
      </div>
    </div>
  )
}

const StageFilter: FC = () => {
  const stages: Array<{ id: number; name: TranslationKey }> = Array.from(
    { length: 24 },
    (_, index) => ({
      id: index + 1,
      name: `stage.${index + 1}` as TranslationKey,
    }),
  )
  const { t } = useTranslation()

  return (
    <div className="grid gap-2 [grid-template-columns:repeat(auto-fit,minmax(200px,1fr))]">
      {stages.map((stage) => (
        <label key={stage.id} className="relative cursor-pointer">
          <input type="checkbox" className="sr-only peer" />
          <span className="block px-3 py-2 text-sm text-center text-gray-700 bg-gray-200 rounded peer-checked:bg-blue-500 peer-checked:text-white hover:bg-gray-300 transition-colors">
            {t(stage.name)}
          </span>
        </label>
      ))}
    </div>
  )
}

export const App: FC = () => {
  return (
    <TranslationLanguageProvider>
      <div className="space-y-2">
        <RulesFilter />
        <ModesFilter />
        <TimeSlotFilter />
        <StageFilter />
      </div>
    </TranslationLanguageProvider>
  )
}
