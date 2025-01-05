import { type FC, type ReactNode, useState } from 'react'
import type { IconType } from 'react-icons'
import { TbHelpHexagon } from 'react-icons/tb'
import { Modal } from './Modal'

export const InputBlock: FC<{
  title: string
  icon: IconType
  children: ReactNode
  help?: ReactNode
}> = ({ title, icon: Icon, children, help }) => {
  const [isOpen, setOpen] = useState(false)

  return (
    <div className="grid gap-4 border-4 border-nord-1 px-4 py-3">
      <div className="flex items-start">
        <div className="flex items-center gap-2">
          <div className="rounded-tr-xl rounded-bl-xl bg-nord-1 p-1.5 text-nord-10">
            <Icon />
          </div>
          <h2 className="font-semibold text-lg text-nord-6">{title}</h2>
          <div className="rounded-tr-xl rounded-bl-xl bg-nord-1 p-1.5 text-nord-10">
            <Icon />
          </div>
        </div>

        {help && (
          <button
            type="button"
            className="ml-auto rounded-lg bg-nord-3 p-1 text-nord-6 transition-colors hover:bg-nord-10"
            onClick={() => setOpen(true)}
          >
            <TbHelpHexagon className="size-5" />
          </button>
        )}
      </div>
      <Modal isOpen={isOpen} onClose={() => setOpen(false)}>
        {help}
      </Modal>
      <div>{children}</div>
    </div>
  )
}
