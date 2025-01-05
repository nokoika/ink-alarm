import type { FC } from 'react'
import { GrDocumentText } from 'react-icons/gr'
import { useTranslation } from '~/hooks/useTranslation'
import { IconButton } from './IconButton'

export const Header: FC = () => {
  const { t } = useTranslation()
  return (
    <header className="flex flex-col items-center gap-x-10 pb-3 md:flex-row">
      <img src="/android-chrome-192x192.png" alt="Inkalarm" className="block" />
      <div className="space-y-3">
        <h1 className="font-bold text-2xl text-gray-800 dark:text-gray-200 ">
          {t('app.name')}
        </h1>
        <div className="w-full text-nord-4 text-sm">
          <p>{t('app.description')}</p>
        </div>
        <div>
          <IconButton
            href={t('label.how_to_use_url')}
            icon={GrDocumentText}
            text={t('label.how_to_use')}
          />
        </div>
      </div>
    </header>
  )
}