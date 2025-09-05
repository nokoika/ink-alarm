module SplaApi.Client
  ( SplaApiClient (..),
    HttpSplaApiClient (..),
    MockSplaApiClient (..),
    newHttpClient,
    newMockClient,
  )
where

import qualified SplaApi (Root, fetchSchedule)
import Prelude (Either, IO, String, return, ($))

-- SplaApi通信を抽象化する型クラス
class SplaApiClient client where
  fetchScheduleData :: client -> IO (Either String SplaApi.Root)

-- 本番用のHTTP通信クライアント
data HttpSplaApiClient = HttpSplaApiClient

instance SplaApiClient HttpSplaApiClient where
  fetchScheduleData _ = SplaApi.fetchSchedule

newHttpClient :: IO HttpSplaApiClient
newHttpClient = return HttpSplaApiClient

-- テスト用のモッククライアント
newtype MockSplaApiClient = MockSplaApiClient
  { mockData :: Either String SplaApi.Root
  }

instance SplaApiClient MockSplaApiClient where
  fetchScheduleData client = return $ mockData client

newMockClient :: Either String SplaApi.Root -> MockSplaApiClient
newMockClient testData = MockSplaApiClient {mockData = testData}
