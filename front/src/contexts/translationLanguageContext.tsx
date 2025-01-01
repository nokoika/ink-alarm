import {
  type FC,
  type ReactNode,
  createContext,
  useContext,
  useState,
} from 'react'
import { Language } from '~/types/querySchema'

type TranslationLanguageContextType = {
  language: Language
  setLanguage: (lang: Language) => void
}

const TranslationLanguageContext = createContext<
  TranslationLanguageContextType | undefined
>(undefined)

type TranslationLanguageProviderProps = {
  children: ReactNode
}

type UseLanguage = {
  language: Language
  setLanguage: (lang: Language) => void
}

const getDefaultLanguage = (): Language => {
  const language = location.pathname.split('/')[1]
  if (language === Language.ja || language === Language.en) {
    return language
  }
  const navigatorLanguage = navigator.language.startsWith('ja')
    ? Language.ja
    : Language.en
  history.replaceState(null, '', `/${navigatorLanguage}`)
  return navigatorLanguage
}

const useLanguage = (): UseLanguage => {
  const [language, setLanguageState] = useState<Language>(getDefaultLanguage)

  const setLanguage = (lang: Language) => {
    setLanguageState(lang)
    history.replaceState(null, '', `/${lang}`)
  }

  return { language, setLanguage }
}

export const TranslationLanguageProvider: FC<
  TranslationLanguageProviderProps
> = ({ children }) => {
  const { language, setLanguage } = useLanguage()

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
