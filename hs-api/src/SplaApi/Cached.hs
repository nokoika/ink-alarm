module SplaApi.Cached (initScheduleCache, fetchScheduleWithCache, ScheduleCache) where

import qualified Data.Cache as C
import qualified SplaApi as S
import SplaApi.Client (SplaApiClient, fetchScheduleData)
import Prelude (Either (Left, Right), IO, Maybe (Just, Nothing), String, return, ($))

type ScheduleCacheKey = ()

scheduleCacheKey :: ScheduleCacheKey
scheduleCacheKey = ()

type ScheduleCache = C.Cache ScheduleCacheKey S.Root

initScheduleCache :: IO ScheduleCache
initScheduleCache = C.newCache (Just 1800) -- 有効期限30分

fetchScheduleWithCache :: (SplaApiClient client) => ScheduleCache -> client -> IO (Either String S.Root)
fetchScheduleWithCache cache client = do
  cachedData <- C.lookup cache scheduleCacheKey
  case cachedData of
    -- キャッシュが存在する場合はキャッシュを返す
    Just value -> return $ Right value
    Nothing -> do
      -- キャッシュがない場合データを取得
      apiRes <- fetchScheduleData client
      case apiRes of
        Left err -> return $ Left err
        Right root -> do
          -- 取得したデータをキャッシュに保存し、取得したデータを返す
          C.insert cache scheduleCacheKey root
          return $ Right root
