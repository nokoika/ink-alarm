import {
  type FC,
  type ReactNode,
  createContext,
  useContext,
  useState,
} from 'react'

type TranslationLanguageContextType = {
  language: 'ja' | 'en'
  setLanguage: (lang: 'ja' | 'en') => void
}

const TranslationLanguageContext = createContext<
  TranslationLanguageContextType | undefined
>(undefined)

type TranslationLanguageProviderProps = {
  children: ReactNode
}

type UseLanguage = {
  language: 'ja' | 'en'
  setLanguage: (lang: 'ja' | 'en') => void
}

const getDefaultLanguage = (): 'ja' | 'en' => {
  const language = location.pathname.split('/')[1]
  if (language === 'ja' || language === 'en') {
    return language
  }
  const navigatorLanguage = navigator.language.startsWith('ja') ? 'ja' : 'en'
  history.replaceState(null, '', `/${navigatorLanguage}`)
  return navigatorLanguage
}

const useLanguage = (): UseLanguage => {
  const [language, setLanguageState] = useState<'ja' | 'en'>(getDefaultLanguage)

  const setLanguage = (lang: 'ja' | 'en') => {
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
