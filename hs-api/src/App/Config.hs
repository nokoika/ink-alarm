{-# LANGUAGE DeriveGeneric #-}

module App.Config
  ( Config (..),
    loadConfig,
  )
where

import Data.Text (Text)
import GHC.Generics (Generic)
import System.Envy
import Prelude (Either (..), IO, Int, Maybe (..), Show, return)

data Config = Config
  { configPort :: Int,
    configCacheTTL :: Int, -- in seconds
    configApiUrl :: Maybe Text
  }
  deriving (Generic, Show)

-- Use generic deriving from Envy
instance FromEnv Config

loadConfig :: IO Config
loadConfig = do
  result <- decodeEnv
  case result of
    Left _err -> return defaultConfig
    Right config -> return config
  where
    defaultConfig =
      Config
        { configPort = 8080,
          configCacheTTL = 1800,
          configApiUrl = Nothing
        }
