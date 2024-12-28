import React from 'react';
import ReactDOM from 'react-dom/client';
import App from './App.tsx';
import './index.css';
import { TranslationLanguageProvider } from './contexts/TranslationLanguageContext.tsx';

ReactDOM.createRoot(document.getElementById('root')!).render(
  <React.StrictMode>
    <TranslationLanguageProvider defaultLanguage="en">
      <App />
    </TranslationLanguageProvider>
  </React.StrictMode>,
);
