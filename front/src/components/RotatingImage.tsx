import { type FC, useState } from 'react'

export const RotatingImage: FC<{
  src: string
  alt: string
}> = ({ src, alt }) => {
  // 現在の回転角度(度数)を管理するステート
  const [rotation, setRotation] = useState(0)

  // 画像がクリックされたときに回転角度を 90 度加算
  const handleClick = () => {
    setRotation((prevRotation) => prevRotation + 360)
  }
  return (
    <button
      type="button"
      onClick={handleClick}
      className="cursor-pointer border-none bg-transparent p-0 transition-transform duration-500"
      style={{ transform: `rotate(${rotation}deg)` }}
    >
      <img src={src} alt={alt} />
    </button>
  )
}
