module Cache (example) where

import qualified Data.Cache as C
import Prelude (IO, Maybe (..), String, putStrLn, ($), (++))

example :: IO ()
example = do
  -- キャッシュを作成（1時間の有効期限）
  let ttl = 3600 -- 1時間 = 3600秒
  cache :: C.Cache String String <- C.newCache (Just ttl)

  -- 値をセット
  C.insert cache "key1" "value1"

  -- 値を取得
  maybeValue <- C.lookup cache "key1"
  case maybeValue of
    Just value -> putStrLn $ "Found: " ++ value
    Nothing -> putStrLn "Not found or expired"

  -- 別の値をセット
  C.insert cache "key2" "value2"

  -- キーを削除
  C.delete cache "key1"

  -- 削除後に再取得
  maybeValueAfterDelete <- C.lookup cache "key1"
  case maybeValueAfterDelete of
    Just value -> putStrLn $ "Found after delete: " ++ value
    Nothing -> putStrLn "Not found after delete"
