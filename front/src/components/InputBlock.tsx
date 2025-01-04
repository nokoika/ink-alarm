import type { FC, ReactNode } from 'react'
import type { IconType } from 'react-icons'

export const InputBlock: FC<{
  title: string
  icon: IconType
  children: ReactNode
}> = ({ title, icon: Icon, children }) => {
  return (
    <div className="grid gap-4 border-4 border-nord-1 px-4 py-3">
      <div className="flex items-center gap-2">
        <div className="rounded-tr-xl rounded-bl-xl bg-nord-1 p-2 text-nord-10">
          <Icon />
        </div>
        <h2 className="font-semibold text-lg text-nord-6">{title}</h2>
        <div className="rounded-tr-xl rounded-bl-xl bg-nord-1 p-2 text-nord-10">
          <Icon />
        </div>
      </div>
      <div>{children}</div>
    </div>
  )
}
