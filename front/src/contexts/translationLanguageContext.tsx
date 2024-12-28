import type React from 'react'
import {
  type Dispatch,
  type ReactNode,
  type SetStateAction,
  createContext,
  useContext,
  useState,
} from 'react'

type TranslationLanguageContextType = {
  language: 'ja' | 'en'
  setLanguage: Dispatch<SetStateAction<'ja' | 'en'>>
}

const TranslationLanguageContext = createContext<
  TranslationLanguageContextType | undefined
>(undefined)

type TranslationLanguageProviderProps = {
  defaultLanguage: 'ja' | 'en'
  children: ReactNode
}

export const TranslationLanguageProvider: React.FC<
  TranslationLanguageProviderProps
> = ({ defaultLanguage, children }) => {
  const [language, setLanguage] = useState<'ja' | 'en'>(defaultLanguage)

  return (
    <TranslationLanguageContext.Provider value={{ language, setLanguage }}>
      {children}
    </TranslationLanguageContext.Provider>
  )
}

export const useTranslationLanguageContext: () => TranslationLanguageContextType =
  () => {
    const context = useContext(TranslationLanguageContext)
    if (!context) {
      throw new Error(
        'useTranslationLanguageContext must be used within a TranslationLanguageProvider',
      )
    }

    return context
  }
