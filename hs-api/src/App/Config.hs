module App.Config
  ( Config (..),
    loadConfig,
  )
where

import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import System.Environment (lookupEnv)
import Text.Read (readMaybe)
import Prelude (IO, Int, Maybe (..), Show, fmap, return, ($), (>>=))

data Config = Config
  { configPort :: Int,
    configCacheTTL :: Int, -- in seconds
    configApiUrl :: Maybe Text
  }
  deriving (Show)

loadConfig :: IO Config
loadConfig = do
  portStr <- lookupEnv "PORT"
  cacheTTLStr <- lookupEnv "CACHE_TTL"
  apiUrlStr <- lookupEnv "API_URL"

  let port = fromMaybe 8080 (portStr >>= readMaybe)
      cacheTTL = fromMaybe 1800 (cacheTTLStr >>= readMaybe)
      apiUrl = fmap T.pack apiUrlStr

  return $
    Config
      { configPort = port,
        configCacheTTL = cacheTTL,
        configApiUrl = apiUrl
      }
