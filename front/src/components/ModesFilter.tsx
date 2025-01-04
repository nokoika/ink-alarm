import type { FC } from 'react'
import { CheckboxList } from '~/components/CheckboxList'
import { useTranslation } from '~/hooks/useTranslation'
import { Mode } from '~/types/querySchema'

export const ModesFilter: FC<{
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
      className="grid grid-cols-2 gap-x-2 md:grid-cols-5"
    />
  )
}
