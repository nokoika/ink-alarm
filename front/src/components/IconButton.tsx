import type { FC } from 'react'
import type { IconType } from 'react-icons'

export const IconButton: FC<{
  onClick: (...args: unknown[]) => unknown
  buttonClass?: string
  icon: IconType
  text: string
}> = ({ onClick, icon: Icon, text, buttonClass }) => {
  return (
    <button
      type="button"
      onClick={onClick}
      className={`px-3 py-2 text-white bg-nord-3 rounded-sm hover:bg-nord-10 transition-colors ${buttonClass ?? ''}`}
    >
      <div className="flex justify-center gap-2">
        <Icon className="block" />
        <p className="text-xs">{text}</p>
      </div>
    </button>
  )
}
