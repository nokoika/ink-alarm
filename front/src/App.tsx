import type { FC } from 'react'
import { type TranslationKey, useTranslation } from './hooks/useTranslation.ts'

const RulesFilter: FC = () => {
  return (
    <div className="flex gap-2">
      {['ガチエリア', 'ガチヤグラ', 'ガチホコバトル', 'ガチアサリ'].map(
        (option) => (
          <label key={option} className="relative cursor-pointer">
            <input type="checkbox" className="sr-only peer" />
            <span className="block px-4 py-2 text-gray-700 bg-gray-200 rounded peer-checked:bg-blue-500 peer-checked:text-white transition-colors duration-200 hover:bg-gray-300">
              {option}
            </span>
          </label>
        ),
      )}
    </div>
  )
}

const ModesFilter: FC = () => {
  return (
    <div className="flex gap-2">
      {[
        'Xマッチ',
        'イベントマッチ',
        'バンカラマッチ (オープン)',
        'バンカラマッチ (チャレンジ)',
        'レギュラーマッチ',
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
  const daysOfWeek = ['月', '火', '水', '木', '金', '土', '日']
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
        {daysOfWeek.map((day) => (
          <label key={day} className="cursor-pointer">
            {/* 実際のチェックボックスは視覚的に隠して、状態によって見た目を変える */}
            <input
              type="checkbox"
              className="sr-only peer"
              // onChangeなどのイベントはあえて入れずに状態管理を排除
            />
            <span className="block px-3 py-2 text-gray-700 bg-gray-200 rounded peer-checked:bg-blue-500 peer-checked:text-white hover:bg-gray-100 transition-colors">
              {day}
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
    <div className="grid grid-cols-4 gap-2">
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
    <div className="space-y-2">
      <RulesFilter />
      <ModesFilter />
      <TimeSlotFilter />
      <StageFilter />
    </div>
  )
}
