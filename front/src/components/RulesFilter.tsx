import type { FC } from 'react'
import { CheckboxList } from '~/components/CheckboxList'
import { useTranslation } from '~/hooks/useTranslation'
import { Rule } from '~/types/querySchema'

export const RulesFilter: FC<{
  rules: Rule[]
  updateRules: (rules: Rule[]) => void
}> = ({ rules, updateRules }) => {
  const { t } = useTranslation()

  const contents: { key: Rule; text: string; enabled: boolean }[] = [
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
    {
      key: Rule.nawabari,
      text: t('rule.turf_war'),
      enabled: rules.includes(Rule.nawabari),
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
      className="grid gap-x-2 grid-cols-2 md:grid-cols-5"
    />
  )
}
