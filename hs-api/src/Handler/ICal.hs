module Handler.ICal
  ( apiV1Handler,
    apiV2Handler,
  )
where

import App.Context (AppContext (..))
import App.Error (AppError (..))
import App.Monad (AppM)
import Control.Monad.Except (throwError)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (ask)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Query
import Service.ICal (generateICalText)
import qualified SplaApi.Cached as Cached
import Prelude (Either (..), String, putStrLn, ($), (++))

-- V1 API handler (raw JSON)
apiV1Handler :: Text -> AppM String
apiV1Handler base64Uri = do
  liftIO $ putStrLn $ "V1 Called With Query: " ++ T.unpack base64Uri
  case Query.parseBase64UrlRaw base64Uri of
    Left err -> throwError $ InvalidQuery (T.pack err)
    Right query -> processQuery query

-- V2 API handler (gzipped JSON)
apiV2Handler :: Text -> AppM String
apiV2Handler base64Uri = do
  liftIO $ putStrLn $ "V2 Called With Query: " ++ T.unpack base64Uri
  case Query.parseBase64UrlGzip base64Uri of
    Left err -> throwError $ InvalidQuery (T.pack err)
    Right query -> processQuery query

-- Common query processing logic
processQuery :: Query.QueryRoot -> AppM String
processQuery query = do
  ctx <- ask
  let cache = acScheduleCache ctx
  scheduleResult <- liftIO $ Cached.fetchScheduleWithCache cache
  case scheduleResult of
    Left err -> throwError $ ApiFetchError (T.pack err)
    Right apiData -> generateICalText query apiData
