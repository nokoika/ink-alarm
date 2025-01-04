import { TZDate } from '@date-fns/tz'
import { format } from 'date-fns'
import { ja } from 'date-fns/locale'
import type { FC } from 'react'
import { useTranslationLanguageContext } from '~/contexts/translationLanguageContext'
import { useTranslation } from '~/hooks/useTranslation'
import { Language } from '~/types/querySchema'

export type Event = {
  id: string
  title: string
  start: Date
  end: Date
}

export const EventList: FC<{ events: Event[]; utcOffset: string }> = ({
  events,
  utcOffset,
}) => {
  const { language } = useTranslationLanguageContext()
  const { t } = useTranslation()

  return (
    <div>
      {events.length === 0 ? (
        <p className="text-nord-5">{t('label.no_schedule')}</p>
      ) : (
        <div className="grid md:grid-cols-3 gap-2">
          {events.map((event) => (
            <div key={event.id} className="px-3 py-2 bg-nord-1 transition">
              <div className="text-sm font-semibold text-nord-9">
                {event.title}
              </div>
              <p className="text-nord-5 text-xs">
                {language === Language.ja && (
                  <>
                    {format(
                      new TZDate(event.start, utcOffset),
                      'M/d (EEE) H:mm',
                      { locale: ja },
                    )}
                    {' ~ '}
                    {format(
                      new TZDate(event.end, utcOffset),
                      'M/d (EEE) H:mm',
                      { locale: ja },
                    )}
                  </>
                )}
                {language === Language.en && (
                  <>
                    {format(
                      new TZDate(event.start, utcOffset),
                      'EEEE, MMMM d, H:mm',
                    )}
                    {' ~ '}
                    {format(
                      new TZDate(event.end, utcOffset),
                      'EEEE, MMMM d, H:mm',
                    )}
                  </>
                )}
              </p>
            </div>
          ))}
        </div>
      )}
    </div>
  )
}
