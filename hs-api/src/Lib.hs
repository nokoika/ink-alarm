module Lib (main) where

import App.Config (Config (..), loadConfig)
import App.Context (AppContext (..))
import Server (server)
import qualified SplaApi.Cached
import qualified Web.Scotty as Scotty
import Prelude (IO, putStrLn, show, ($), (++))

main :: IO ()
main = do
  -- Load configuration from environment variables
  config <- loadConfig
  putStrLn $ "Starting server on port " ++ show (configPort config)

  -- Initialize dependencies
  scheduleCache <- SplaApi.Cached.initScheduleCache

  -- Create application context
  let ctx =
        AppContext
          { acScheduleCache = scheduleCache,
            acConfig = config
          }

  -- Start the server with the configured port
  Scotty.scotty (configPort config) (server ctx)
