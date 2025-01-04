import { type ChangeEvent, type FC, useState } from 'react'
import { FiChevronDown } from 'react-icons/fi'
import { useTimezoneSelect } from 'react-timezone-select'

const convertOffsetToString = (offsetNumber: number) => {
  // 符号判定
  const sign = offsetNumber < 0 ? '-' : '+'

  // 絶対値を取得
  const abs = Math.abs(offsetNumber)

  // 時・分に分解 (3.5h => 3h + 0.5h(=30min))
  const hours = Math.floor(abs)
  const minutes = Math.round((abs - hours) * 60)

  // 2桁のゼロ埋め文字列に
  const hh = String(hours).padStart(2, '0')
  const mm = String(minutes).padStart(2, '0')

  return `${sign}${hh}:${mm}`
}

export const UtcOffset: FC<{
  utcOffset: string
  setUtcOffset: (offset: string) => void
}> = ({ utcOffset, setUtcOffset }) => {
  const [timezone, setTimezone] = useState<string>(
    Intl.DateTimeFormat().resolvedOptions().timeZone,
  )

  const { options, parseTimezone } = useTimezoneSelect({
    displayValue: 'UTC',
    labelStyle: 'offsetHidden',
  })

  const selected: string | undefined = options.find(
    (option) =>
      option.offset &&
      convertOffsetToString(option.offset) === utcOffset &&
      option.value === timezone,
  )?.value

  const onChange = (e: ChangeEvent<HTMLSelectElement>) => {
    const parsed = parseTimezone(e.target.value)
    setTimezone(parsed.value)
    if (parsed.offset) {
      setUtcOffset(convertOffsetToString(parsed.offset))
    }
  }

  return (
    <div className="flex gap-4 flex-col md:items-center md:flex-row">
      <div className="text-sm font-medium">UTC {utcOffset}</div>
      <div className="relative border-0 border-b border-nord-3 pr-6 ">
        <select
          onChange={onChange}
          value={selected ?? 'none'}
          className="appearance-none bg-transparent text-sm focus:outline-none focus:ring-0 focus:border-nord-10 max-w-40"
        >
          <option
            value="none"
            disabled={true}
            className="text-sm bg-nord-2 p-1 appearance-none"
          >
            -
          </option>
          {options.map(
            (option) =>
              !!option.offset && (
                <option
                  key={option.value}
                  value={option.value}
                  className="text-sm bg-nord-2 p-1 appearance-none"
                >
                  {option.label}
                </option>
              ),
          )}
        </select>
        <FiChevronDown
          className="absolute right-0 bottom-1/2 transform translate-y-1/2 text-nord-8 pointer-events-none"
          size={16}
        />
      </div>
    </div>
  )
}
