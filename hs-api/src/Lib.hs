module Lib (main) where

import App.Config (Config (..))
import App.Context (AppContext (..))
import Server (server)
import qualified SplaApi.Cached
import qualified SplaApi.Client as SplaClient
import qualified Web.Scotty as Scotty
import Prelude (IO, putStrLn)

main :: IO ()
main = do
  putStrLn "Starting server on port 8080"

  -- Initialize dependencies
  scheduleCache <- SplaApi.Cached.initScheduleCache
  splaClient <- SplaClient.newHttpClient

  -- Create application context
  let ctx =
        AppContext
          { acScheduleCache = scheduleCache,
            acConfig = Config {configPort = 8080},
            acHttpSplaApiClient = splaClient
          }

  -- Start the server on port 8080
  Scotty.scotty 8080 (server ctx)
