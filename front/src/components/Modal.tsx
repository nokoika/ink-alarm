import { type FC, type ReactNode, useEffect } from 'react'
import { IoCloseSharp } from 'react-icons/io5'

export const Modal: FC<{
  isOpen: boolean
  onClose: () => void
  children: ReactNode
}> = ({ isOpen, onClose, children }) => {
  // スクロールの無効化
  useEffect(() => {
    const disableScroll = (e: Event) => {
      e.preventDefault()
      e.stopPropagation()
      return false
    }
    const disableScrollKey = (e: KeyboardEvent) => {
      const keysToDisable = [
        'ArrowUp',
        'ArrowDown',
        'PageUp',
        'PageDown',
        'Home',
        'End',
        ' ',
      ]
      if (keysToDisable.includes(e.key)) {
        e.preventDefault()
      }
    }

    if (isOpen) {
      window.addEventListener('wheel', disableScroll, { passive: false }) // マウススクロール無効
      window.addEventListener('touchmove', disableScroll, { passive: false }) // タッチスクロール無効
      window.addEventListener('keydown', disableScrollKey) // キーボードスクロール無効
    } else {
      window.removeEventListener('wheel', disableScroll)
      window.removeEventListener('touchmove', disableScroll)
      window.removeEventListener('keydown', disableScrollKey)
    }

    return () => {
      window.removeEventListener('wheel', disableScroll)
      window.removeEventListener('touchmove', disableScroll)
      window.removeEventListener('keydown', disableScrollKey)
    }
  }, [isOpen])

  if (!isOpen) {
    return null
  }

  return (
    <div
      className="fixed inset-0 z-50 flex items-center justify-center bg-black bg-opacity-50"
      onClick={onClose}
      onKeyDown={(e) => e.key === 'Escape' && onClose()}
    >
      <div
        className="relative rounded-lg bg-nord-4 p-6 text-nord-2 shadow-lg"
        onClick={(e) => e.stopPropagation()}
        onKeyDown={() => {
          /* キー入力については stopPropagation しない */
        }}
      >
        <button
          type="button"
          className="absolute top-2 right-2"
          onClick={onClose}
        >
          <IoCloseSharp />
        </button>
        <div className="text-left text-sm">{children}</div>
      </div>
    </div>
  )
}
