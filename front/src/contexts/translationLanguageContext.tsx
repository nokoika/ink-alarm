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

const setBrowserLanguage = (lang: Language) => {
  history.replaceState(null, '', `/${lang}`)
  const html = document.querySelector('html')
  if (html) {
    html.setAttribute('lang', lang)
  }
  const title = document.querySelector('title')
  if (title) {
    title.innerHTML = lang === Language.ja ? 'ガチアラーム' : 'Ink Alarm'
  }
}

const getDefaultLanguage = (): Language => {
  const language = location.pathname.split('/')[1]
  if (language === Language.ja || language === Language.en) {
    return language
  }
  const navigatorLanguage = navigator.language.startsWith('ja')
    ? Language.ja
    : Language.en
  setBrowserLanguage(navigatorLanguage)
  return navigatorLanguage
}

const useLanguage = (): UseLanguage => {
  const [language, setLanguageState] = useState<Language>(getDefaultLanguage)

  const setLanguage = (lang: Language) => {
    setLanguageState(lang)
    setBrowserLanguage(lang)
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
