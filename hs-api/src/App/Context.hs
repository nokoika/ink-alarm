module App.Context
  ( AppContext (..),
  )
where

import App.Config (Config)
import qualified SplaApi.Cached as Cached

data AppContext = AppContext
  { acScheduleCache :: Cached.ScheduleCache,
    acConfig :: Config
  }
