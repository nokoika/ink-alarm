module Lib (main) where

import Control.Monad.IO.Class (liftIO)
import Data.Function ((&)) -- js でいうところの pipeline operator
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Filter
import qualified ICal
import qualified Network.HTTP.Conduit as H
import qualified Network.HTTP.Types.Status as Status
import qualified Query
import qualified SplaApi
import qualified SplaApi.Cached
import qualified Web.Scotty as Scotty
import Prelude (IO, String, either, putStrLn, ($), (++), (>>=))

main :: IO ()
main = do
  putStrLn "Starting server on port 8080"
  client <- H.newManager H.tlsManagerSettings
  scheduleCache <- SplaApi.Cached.initScheduleCache
  Scotty.scotty 8080 $ do
    Scotty.get "/api/ical" $
      handleApiV1 client scheduleCache
    Scotty.get "/api/v2/ical" $
      handleApiV2 client scheduleCache

handleApiV1 :: H.Manager -> SplaApi.Cached.ScheduleCache -> Scotty.ActionM ()
handleApiV1 client scheduleCache = do
  base64Uri <- Scotty.queryParam "query" :: Scotty.ActionM T.Text
  liftIO $ putStrLn $ "V1 Called With Query: " ++ T.unpack base64Uri
  either
    sendClientError
    (processQuery client scheduleCache base64Uri)
    (Query.parseBase64UrlRaw base64Uri)

handleApiV2 :: H.Manager -> SplaApi.Cached.ScheduleCache -> Scotty.ActionM ()
handleApiV2 client scheduleCache = do
  base64Uri <- Scotty.queryParam "query" :: Scotty.ActionM T.Text
  liftIO $ putStrLn $ "V2 Called With Query: " ++ T.unpack base64Uri
  either
    sendClientError
    (processQuery client scheduleCache base64Uri)
    (Query.parseBase64UrlGzip base64Uri)

processQuery :: H.Manager -> SplaApi.Cached.ScheduleCache -> T.Text -> Query.QueryRoot -> Scotty.ActionM ()
processQuery client scheduleCache base64Uri query =
  liftIO (SplaApi.Cached.fetchScheduleWithCache client scheduleCache)
    >>= either sendInternalError (generateICal base64Uri query)

generateICal :: T.Text -> Query.QueryRoot -> SplaApi.Root -> Scotty.ActionM ()
generateICal base64Uri query SplaApi.Root {result = apiResult} =
  Filter.createICalInput settingsUrl query apiResult
    & ICal.buildICalText
    & sendResponse
  where
    settingsUrl = buildSettingsUrl (Query.language query) base64Uri

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

buildSettingsUrl :: Query.Language -> T.Text -> String
buildSettingsUrl language base64Uri =
  "https://ink-alarm.pages.dev/"
    ++ languagePath language
    ++ "?query="
    ++ T.unpack base64Uri

languagePath :: Query.Language -> String
languagePath Query.Japanese = "ja"
languagePath Query.English = "en"
