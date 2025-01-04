import type { FC } from 'react'
import { Input } from '~/components/Input'
import { TranslationLanguageProvider } from '~/contexts/translationLanguageContext'

export const App: FC = () => {
  return (
    <TranslationLanguageProvider>
      <div className="bg-nord-0 min-h-screen text-nord-4">
        <Input />
      </div>
    </TranslationLanguageProvider>
  )
}
