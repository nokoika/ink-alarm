module Server
  ( server,
  )
where

import App.Context (AppContext)
import App.Error (AppError (..), toHttpError)
import App.Monad (AppM, runAppM)
import Control.Monad.IO.Class (liftIO)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import Handler.ICal (apiV1Handler, apiV2Handler)
import qualified Network.HTTP.Types.Status as Status
import qualified Web.Scotty as Scotty
import Prelude (Either (..), String, putStrLn, ($), (++))

-- Main server setup with routes
server :: AppContext -> Scotty.ScottyM ()
server ctx = do
  Scotty.get "/api/ical" $ do
    base64Uri <- Scotty.queryParam "query" :: Scotty.ActionM Text
    handleRequest ctx (apiV1Handler base64Uri)

  Scotty.get "/api/v2/ical" $ do
    base64Uri <- Scotty.queryParam "query" :: Scotty.ActionM Text
    handleRequest ctx (apiV2Handler base64Uri)

-- Run AppM action and handle results
handleRequest :: AppContext -> AppM String -> Scotty.ActionM ()
handleRequest ctx appAction = do
  result <- liftIO $ runAppM ctx appAction
  case result of
    Left err -> sendError err
    Right iCalText -> sendICalResponse iCalText

-- Send error response based on AppError type
sendError :: AppError -> Scotty.ActionM ()
sendError err = do
  let (status, msg) = toHttpError err
  liftIO $ putStrLn $ "Error: " ++ T.unpack msg
  Scotty.status status
  Scotty.text $ TL.fromStrict msg

-- Send successful iCal response
sendICalResponse :: String -> Scotty.ActionM ()
sendICalResponse iCalText = do
  Scotty.status Status.ok200
  Scotty.setHeader "Content-Type" "text/calendar; charset=utf-8"
  Scotty.setHeader "Access-Control-Allow-Origin" "*"
  Scotty.setHeader "Cache-Control" "no-cache"
  Scotty.text $ TL.pack iCalText
