module Lib (main) where

import Control.Monad.IO.Class (liftIO)
import Data.Function ((&)) -- js でいうところの pipeline operator
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Filter
import qualified ICal
import qualified Network.HTTP.Types.Status as Status
import qualified Query
import qualified SplaApi
import qualified SplaApi.Cached
import qualified Web.Scotty as Scotty
import Prelude (IO, String, either, putStrLn, ($), (++), (>>=))

main :: IO ()
main = do
  putStrLn "Starting server on port 8080"
  scheduleCache <- SplaApi.Cached.initScheduleCache
  Scotty.scotty 8080 $ do
    Scotty.get "/api/ical" $
      handleApiV1 scheduleCache
    Scotty.get "/api/v2/ical" $
      handleApiV2 scheduleCache

-- API を受け付けて Query をパースする
handleApiV1 :: SplaApi.Cached.ScheduleCache -> Scotty.ActionM ()
handleApiV1 scheduleCache = do
  base64Uri <- Scotty.queryParam "query" :: Scotty.ActionM T.Text
  liftIO $ putStrLn $ "V1 Called With Query: " ++ T.unpack base64Uri
  either sendClientError (processQuery scheduleCache) (Query.parseBase64UrlRaw base64Uri)

handleApiV2 :: SplaApi.Cached.ScheduleCache -> Scotty.ActionM ()
handleApiV2 scheduleCache = do
  base64Uri <- Scotty.queryParam "query" :: Scotty.ActionM T.Text
  liftIO $ putStrLn $ "V2 Called With Query: " ++ T.unpack base64Uri
  either sendClientError (processQuery scheduleCache) (Query.parseBase64UrlGzip base64Uri)

processQuery :: SplaApi.Cached.ScheduleCache -> Query.QueryRoot -> Scotty.ActionM ()
processQuery scheduleCache query =
  liftIO (SplaApi.Cached.fetchScheduleWithCache scheduleCache)
    >>= either sendInternalError (generateICal query)

generateICal :: Query.QueryRoot -> SplaApi.Root -> Scotty.ActionM ()
generateICal query SplaApi.Root {result = apiResult} =
  Filter.createICalInput query apiResult
    & ICal.buildICalText
    & sendResponse

sendClientError :: String -> Scotty.ActionM ()
sendClientError err = do
  liftIO $ putStrLn $ "Invalid Request: " ++ err
  Scotty.status Status.badRequest400
  Scotty.text "Invalid request"

sendInternalError :: String -> Scotty.ActionM ()
sendInternalError err = do
  liftIO $ putStrLn $ "Internal Server Error: " ++ err
  Scotty.status Status.internalServerError500
  Scotty.text "Internal server error"

sendResponse :: String -> Scotty.ActionM ()
sendResponse text = do
  Scotty.status Status.ok200
  Scotty.setHeader "Content-Type" "text/calendar; charset=utf-8"
  Scotty.setHeader "Access-Control-Allow-Origin" "*"
  Scotty.setHeader "Cache-Control" "no-cache"
  Scotty.text $ TL.pack text
