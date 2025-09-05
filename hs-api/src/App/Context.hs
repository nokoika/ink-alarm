module App.Context
  ( AppContext (..),
  )
where

import App.Config (Config)
import qualified SplaApi.Cached as Cached
import qualified SplaApi.Client as SplaClient

data AppContext = AppContext
  { acScheduleCache :: Cached.ScheduleCache,
    acConfig :: Config,
    acHttpSplaApiClient :: SplaClient.HttpSplaApiClient
  }
