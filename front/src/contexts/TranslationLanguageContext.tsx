import React, {
  createContext,
  useContext,
  useState,
  ReactNode,
  Dispatch,
  SetStateAction,
} from 'react';

type TranslationLanguageContextType = {
  language: 'ja' | 'en';
  setLanguage: Dispatch<SetStateAction<'ja' | 'en'>>;
};

const defaultValue: TranslationLanguageContextType = {
  language: 'ja',
  setLanguage: () => {},
};

const TranslationLanguageContext =
  createContext<TranslationLanguageContextType>(defaultValue);

type TranslationLanguageProviderProps = {
  defaultLanguage: 'ja' | 'en';
  children: ReactNode;
};

export const TranslationLanguageProvider: React.FC<
  TranslationLanguageProviderProps
> = ({ defaultLanguage, children }) => {
  const [language, setLanguage] = useState<'ja' | 'en'>(defaultLanguage);

  return (
    <TranslationLanguageContext.Provider value={{ language, setLanguage }}>
      {children}
    </TranslationLanguageContext.Provider>
  );
};

export const useTranslationLanguageContext: () => TranslationLanguageContextType =
  () => {
    return useContext(TranslationLanguageContext);
  };
