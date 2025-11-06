import pako from 'pako'

const BASE64_PADDING_REGEX = /=+$/g
const BASE64_URL_DASH_REGEX = /-/g
const BASE64_URL_UNDERSCORE_REGEX = /_/g
const BASE64_PLUS_REGEX = /\+/g
const BASE64_SLASH_REGEX = /\//g

const normalizeBase64Url = (value: string): string => {
  const paddingLength = (4 - (value.length % 4)) % 4
  return (
    value
      .replace(BASE64_URL_DASH_REGEX, '+')
      .replace(BASE64_URL_UNDERSCORE_REGEX, '/') + '='.repeat(paddingLength)
  )
}

const decodeBase64Url = (value: string): Uint8Array => {
  const normalized = normalizeBase64Url(value)
  const binary = atob(normalized)
  const bytes = new Uint8Array(binary.length)
  for (let index = 0; index < binary.length; index += 1) {
    bytes[index] = binary.charCodeAt(index)
  }
  return bytes
}

const encodeBytesToBase64Url = (bytes: Uint8Array): string => {
  const base64 = btoa(String.fromCharCode(...bytes))
  return base64
    .replace(BASE64_PADDING_REGEX, '')
    .replace(BASE64_PLUS_REGEX, '-')
    .replace(BASE64_SLASH_REGEX, '_')
}

export const decodeBase64UrlToJson = (value: string): string => {
  const bytes = decodeBase64Url(value)
  try {
    return pako.ungzip(bytes, { to: 'string' }) as string
  } catch (_gzipError) {
    return new TextDecoder().decode(bytes)
  }
}

export const encodeJsonToBase64Url = (json: string): string => {
  const gzip = pako.gzip(json)
  return encodeBytesToBase64Url(gzip)
}
