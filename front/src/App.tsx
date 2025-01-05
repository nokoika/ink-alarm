import type { FC } from 'react'
import { Footer } from '~/components/Footer'
import { Header } from '~/components/Header'
import { Input } from '~/components/Input'
import { TranslationLanguageProvider } from '~/contexts/translationLanguageContext'

export const App: FC = () => {
  return (
    <TranslationLanguageProvider>
      <div className="min-h-screen bg-nord-0 text-nord-4 ">
        <div className="mx-auto max-w-5xl p-4">
          <Header />
          <Input />
          <Footer />
        </div>
      </div>
    </TranslationLanguageProvider>
  )
}
