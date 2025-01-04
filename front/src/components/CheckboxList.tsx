import { FiCheckSquare, FiSquare } from 'react-icons/fi'

// tsx 文法の中では、arrow だと generics が使えないため function で書く
export function CheckboxList<T extends string | number>({
  contents,
  updateItem,
  radio = false,
  className,
}: {
  contents: { key: T; text: string; enabled: boolean }[]
  updateItem: (key: T, enabled: boolean) => void
  radio?: boolean
  className?: string
}) {
  return (
    <div className={className}>
      {contents.map(({ key, text, enabled }) => (
        <label key={key} className="flex min-h-10 cursor-pointer items-center">
          <input
            type={radio ? 'radio' : 'checkbox'}
            className="peer hidden"
            checked={enabled}
            onChange={() => {
              updateItem(key, !enabled)
            }}
          />
          <span className="flex h-5 w-5 items-center justify-center text-nord-3 transition-colors peer-checked:text-nord-8">
            {enabled ? <FiCheckSquare size={20} /> : <FiSquare size={20} />}
          </span>
          <span className="ml-2 text-sm">{text}</span>
        </label>
      ))}
    </div>
  )
}
