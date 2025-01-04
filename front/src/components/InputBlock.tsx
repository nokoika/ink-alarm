import type { FC, ReactNode } from 'react'
import type { IconType } from 'react-icons'

export const InputBlock: FC<{
  title: string
  icon: IconType
  children: ReactNode
}> = ({ title, icon: Icon, children }) => {
  return (
    <div className="grid gap-4 border-4 px-4 py-3 border-nord-1">
      <div className="flex items-center gap-2">
        <div className="text-nord-10 bg-nord-1 p-2 rounded-bl-xl rounded-tr-xl">
          <Icon />
        </div>
        <h2 className="text-lg font-semibold text-nord-6">{title}</h2>
        <div className="text-nord-10 bg-nord-1 p-2 rounded-bl-xl rounded-tr-xl">
          <Icon />
        </div>
      </div>
      <div>{children}</div>
    </div>
  )
}
