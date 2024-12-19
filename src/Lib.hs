module Lib (main) where

import Control.Monad.IO.Class (liftIO)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Filter
import qualified ICal
import qualified Query
import qualified SplaApi
import qualified SplaApi.Cached
import qualified Web.Scotty as Scotty
import Prelude (IO, const, either, putStrLn, ($))

-- - (できた) リクエスト内容の受理
-- - (できた) 全スケジュールを確認するために Spla API を叩く
--   - (まだ) リクエストした内容は cloudrun 内にキャッシュする。有効期限も 30 分程度でセットする
--   - (まだ) インメモリキャッシュがある場合はリクエスト自体スキップ
-- - (できた) 必要なスケジュールだけになるよう filter する
--   - request query によって、ステージ名、ルール名、時間帯を絞る
--   - 条件は複雑なので base64URL でエンコードしてリクエストする
-- - (できた) iCal 形式のテキストを生成する
-- - response を返す

main :: IO ()
main = do
  putStrLn "Starting server on port 8080"

  scheduleCache :: SplaApi.Cached.ScheduleCache <- SplaApi.Cached.initScheduleCache

  Scotty.scotty 8080 $ do    -- 
    Scotty.get "/api" $ handleApi scheduleCache
  where
    handleApi :: SplaApi.Cached.ScheduleCache -> Scotty.ActionM ()
    handleApi scheduleCache = do
      -- クエリの取得とデコード
      base64Uri <- Scotty.queryParam "query" :: Scotty.ActionM T.Text
      either (const $ Scotty.text "Invalid query") (handleQuery scheduleCache) (Query.parseBase64Url base64Uri)

    -- クエリが有効な場合の処理
    handleQuery :: SplaApi.Cached.ScheduleCache -> Query.QueryRoot -> Scotty.ActionM ()
    handleQuery scheduleCache query = do
      -- APIからデータを取得
      apiRes <- liftIO $ SplaApi.Cached.fetchScheduleWithCache scheduleCache
      either (const $ Scotty.text "Failed to fetch schedules") (handleSchedule query) apiRes

    -- スケジュールデータが取得できた場合の処理
    handleSchedule :: Query.QueryRoot -> SplaApi.Root -> Scotty.ActionM ()
    handleSchedule query SplaApi.Root {result} = do
      let filteredSchedules = Filter.createIcalInput query result
          iCalText = ICal.buildICalText filteredSchedules
      Scotty.text $ TL.pack iCalText
