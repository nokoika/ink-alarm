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
        <label key={key} className="flex items-center cursor-pointer min-h-10">
          <input
            type={radio ? 'radio' : 'checkbox'}
            className="hidden peer"
            checked={enabled}
            onChange={() => {
              updateItem(key, !enabled)
            }}
          />
          <span className="flex items-center justify-center w-5 h-5 text-nord-3 peer-checked:text-nord-8 transition-colors">
            {enabled ? <FiCheckSquare size={20} /> : <FiSquare size={20} />}
          </span>
          <span className="ml-2 text-sm ">{text}</span>
        </label>
      ))}
    </div>
  )
}
