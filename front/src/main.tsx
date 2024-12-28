import React from 'react'
import reactDom from 'react-dom/client'
import { App } from '~/App.tsx'
import './index.css'
import { TranslationLanguageProvider } from '~/contexts/translationLanguageContext'

const root = document.getElementById('root')

if (!root) {
  throw new Error('root element not found')
}

reactDom.createRoot(root).render(
  <React.StrictMode>
    <TranslationLanguageProvider defaultLanguage="en">
      <App />
    </TranslationLanguageProvider>
  </React.StrictMode>,
)
