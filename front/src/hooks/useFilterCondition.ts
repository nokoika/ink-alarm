import { useState } from 'react'
import type { FilterConditionWithKey } from '~/types/propTypes'
import { generateDefaultFilter } from '~/utils/generateInitialState'

type UseFilterCondition = {
  filters: FilterConditionWithKey[]
  addFilterAfter: (key: string) => void
  updateFilter: (
    key: string,
    newFilter: Partial<FilterConditionWithKey>,
  ) => void
  removeFilter: (key: string) => void
}
export const useFilterCondition = (): UseFilterCondition => {
  const [filters, setFilters] = useState<FilterConditionWithKey[]>([
    generateDefaultFilter(),
  ])

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

  return {
    filters,
    addFilterAfter,
    updateFilter,
    removeFilter,
  }
}
