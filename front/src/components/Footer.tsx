import type { FC } from 'react'

export const Footer: FC = () => {
  return (
    <footer className="pt-10">
      <div className="border-nord-1 border-t-4 py-2 text-center text-gray-500 text-xs">
        <p>
          <a
            href="https://x.com/ringomotintin"
            target="_blank"
            rel="noopener noreferrer"
            className="text-nord-10 italic underline"
          >
            @ringomotintin
          </a>{' '}
          |{' '}
          <a
            href="https://github.com/nokoika/ink-alarm"
            target="_blank"
            rel="noopener noreferrer"
            className="text-nord-10 italic underline"
          >
            GitHub Repository
          </a>
        </p>
      </div>
    </footer>
  )
}
