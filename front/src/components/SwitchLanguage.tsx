import type { FC } from 'react'
import { CheckboxList } from '~/components/CheckboxList'
import { useTranslation } from '~/hooks/useTranslation'
import { Language } from '~/types/querySchema'

export const SwitchLanguage: FC<{
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
      className="grid gap-x-2 grid-cols-2"
    />
  )
}
