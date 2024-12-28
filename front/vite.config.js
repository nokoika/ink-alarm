// @ts-check

import react from '@vitejs/plugin-react-swc'
import { defineConfig } from 'vite'

// https://vitejs.dev/config/
export default defineConfig({
  plugins: [react()],
  server: { port: 40000 },
  resolve: {
    alias: { '~/': `${__dirname}/src/` },
  },
  build: {
    rollupOptions: {
      input: {
        index: 'index.html',
        ja: 'ja.html',
        en: 'en.html',
      },
    },
  },
})
