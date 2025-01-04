import type { FC } from 'react'
import type { IconType } from 'react-icons'

export const IconButton: FC<{
  onClick: (...args: unknown[]) => unknown
  buttonColorClass?: string
  icon: IconType
  text: string
}> = ({ onClick, icon: Icon, text, buttonColorClass }) => {
  return (
    <button
      type="button"
      onClick={onClick}
      className={`px-3 py-2 text-white rounded-sm hover:bg-nord-10 transition-colors ${buttonColorClass ?? 'bg-nord-3'}`}
    >
      <div className="flex justify-center gap-2">
        <Icon className="block" />
        <p className="text-xs">{text}</p>
      </div>
    </button>
  )
}
