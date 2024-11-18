module Lib (generateICalEvents, someFunc) where

import Control.Exception (SomeException, try)
import Data.Aeson (FromJSON, decode)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base64.URL as BU (decode)
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.Char8 as L8
import qualified Data.Text as T (Text, pack)
import qualified Data.Text.Encoding as TE (decodeUtf8', encodeUtf8)
import qualified Data.Text.Lazy as TL (fromStrict)
import GHC.Generics (Generic)
import qualified ICal (ICalEvent (..), Reminder (..), ReminderAction (..), ReminderTrigger (..), buildICalText)
import Network.HTTP.Conduit (simpleHttp)
import Query (FilterCondition (..), NotificationSetting (..), QueryRoot (..), StageFilter (..), TimeSlot (..))
import qualified Web.Scotty as S (ActionM, get, queryParam, scotty, text)
import Prelude (Bool (False), Either (Left, Right), IO, Int, Maybe (Just, Nothing), Show, String, ($), (++), (>>=))
import qualified Prelude as P (print, putStrLn, return, show)

-- 入力クエリを受け取り、HTTPリクエストを行い、ICalEventのリストを返す
generateICalEvents ::
  QueryRoot -> -- 入力クエリ
  IO [ICal.ICalEvent] -- HTTPリクエストを行い、ICalEventのリストを返す
generateICalEvents query = do
  -- ここにHTTPリクエストを行う処理を書く
  -- 仮にダミーのデータを返す
  P.return
    [ ICal.ICalEvent
        { summary = "スプラトゥーン2 レギュラーマッチ",
          description = "スプラトゥーン2のレギュラーマッチが開催されます。",
          start = "2020-01-01T00:00:00Z",
          end = "2020-01-01T01:00:00Z",
          url = Just "https://example.com",
          reminders =
            [ ICal.Reminder
                { trigger = ICal.ReminderTrigger {time = 15},
                  action = ICal.Display
                },
              ICal.Reminder
                { trigger = ICal.ReminderTrigger {time = 1},
                  action = ICal.Email
                }
            ]
        },
      ICal.ICalEvent
        { summary = "スプラトゥーン2 ガチエリア",
          description = "スプラトゥーン2のガチエリアが開催されます。",
          start = "2020-01-01T01:00:00Z",
          end = "2020-01-01T02:00:00Z",
          url = Just "https://example.com",
          reminders =
            [ ICal.Reminder
                { trigger = ICal.ReminderTrigger {time = 10},
                  action = ICal.Display
                },
              ICal.Reminder
                { trigger = ICal.ReminderTrigger {time = 30},
                  action = ICal.Display
                }
            ]
        }
    ]

someFunc :: IO ()

hoge = do
  let query =
        QueryRoot
          { language = "ja",
            utcOffset = "+09:00", 
            filters =
              [ FilterCondition
                  { matchType = "regular",
                    stages = Just $ StageFilter {matchBothStages = False, stageIds = [1, 2]},
                    rules = Just ["TURF_WAR"],
                    timeSlots = Just [TimeSlot {start = "00:00", end = "01:00", dayOfWeek = Just "Monday"}],
                    notifications = Just [NotificationSetting {minutesBefore = 15}]
                  }
              ]
          }
  events <- generateICalEvents query
  P.putStrLn $ ICal.buildICalText events (language query)

fuga =
  S.scotty 3000 $
    S.get "/decode" $
      (S.queryParam "data" :: S.ActionM T.Text) >>= \base64Url -> S.text $ case decodeBase64UriToJson $ TE.encodeUtf8 base64Url of
        Left err -> TL.fromStrict $ T.pack err
        Right decodedText -> TL.fromStrict decodedText

decodeBase64UriToJson :: BS.ByteString -> Either String T.Text
decodeBase64UriToJson base64Url = case BU.decode base64Url of
  Left err -> Left $ P.show err
  Right decoded -> case TE.decodeUtf8' decoded of
    Left err -> Left $ P.show err
    Right decodedText -> Right decodedText

data Person = Person
  { name :: String,
    age :: Int
  }
  deriving (Show, Generic)

instance FromJSON Person

piyo = do
  let jsonString :: T.Text
      jsonString = "{\"name\":\"Charlie\",\"age\":28}"

  -- Text を ByteString に変換してから decode する
  let person = decode (BL.fromStrict $ TE.encodeUtf8 jsonString) :: Maybe Person
  case person of
    Just p -> P.print p
    Nothing -> P.putStrLn "Failed to parse JSON"

someFunc = do
  let url = "https://spla3.yuu26.com/api/schedule"
  response <- try (simpleHttp url) :: IO (Either SomeException L8.ByteString)
  case response of
    Left err -> P.putStrLn $ "Error: " ++ P.show err
    Right json -> L8.putStrLn json
