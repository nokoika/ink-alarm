import type { FC } from 'react'
import { CheckboxList } from '~/components/CheckboxList'
import { type TranslationKey, useTranslation } from '~/hooks/useTranslation'
import type { StageFilter } from '~/types/querySchema'

export const StagesFilter: FC<{
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
        className="grid gap-x-2 grid-cols-2 md:grid-cols-5"
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
        className="grid gap-x-2 md:grid-cols-2"
      />
    </div>
  )
}
