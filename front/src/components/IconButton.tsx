import type { FC, ReactNode } from 'react'
import type { IconType } from 'react-icons'

const Button: FC<{
  onClick?: (...args: unknown[]) => unknown
  children: ReactNode
  buttonColorClass?: string
}> = ({ onClick, children, buttonColorClass }) => {
  return (
    <button
      type="button"
      onClick={onClick}
      className={`rounded-sm px-3 py-2 text-white transition-colors hover:bg-nord-10 ${buttonColorClass ?? 'bg-nord-3'}`}
    >
      {children}
    </button>
  )
}

const Anchor: FC<{
  href: string
  children: ReactNode
  buttonColorClass?: string
}> = ({ href, children, buttonColorClass }) => {
  return (
    <a
      href={href}
      target="_blank"
      rel="noopener noreferrer"
      className={`inline-block rounded-sm px-3 py-2 text-white transition-colors hover:bg-nord-10 ${buttonColorClass ?? 'bg-nord-3'}`}
    >
      {children}
    </a>
  )
}

type Props = {
  buttonColorClass?: string
  icon: IconType
  text: string
} & (
  | {
      onClick: (...args: unknown[]) => unknown
    }
  | {
      href: string
    }
)

export const IconButton: FC<Props> = (props) => {
  // a タグか button タグかを切り替える
  const Component: FC<{ children: ReactNode }> =
    'href' in props
      ? ({ children }) => <Anchor {...props}>{children}</Anchor>
      : ({ children }) => <Button {...props}>{children}</Button>
  const Icon = props.icon

  return (
    <Component>
      <div className="flex justify-center gap-2">
        <Icon className="block" />
        <p className="text-xs">{props.text}</p>
      </div>
    </Component>
  )
}
