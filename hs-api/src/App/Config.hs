module App.Config
  ( Config (..),
    loadConfig,
  )
where

import Data.Maybe (fromMaybe)
import System.Environment (lookupEnv)
import Text.Read (readMaybe)
import Prelude (IO, Int, Show, return, ($), (>>=))

data Config = Config
  { configPort :: Int
  }
  deriving (Show)

loadConfig :: IO Config
loadConfig = do
  portStr <- lookupEnv "PORT"

  let port = fromMaybe 8080 (portStr >>= readMaybe)

  return $
    Config
      { configPort = port
      }
