module Lib (someFunc) where

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

someFunc :: IO ()
someFunc = do
  P.putStrLn "Hello, world!"

--
-- hoge = do
--   let query =
--         QueryRoot
--           { language = "ja",
--             utcOffset = "+09:00",
--             filters =
--               [ FilterCondition
--                   { matchType = "regular",
--                     stages = Just $ StageFilter {matchBothStages = False, stageIds = [1, 2]},
--                     rules = Just ["TURF_WAR"],
--                     timeSlots = Just [TimeSlot {start = "00:00", end = "01:00", dayOfWeek = Just "Monday"}],
--                     notifications = Just [NotificationSetting {minutesBefore = 15}]
--                   }
--               ]
--           }
--   events <- generateICalEvents query
--   P.putStrLn $ ICal.buildICalText events (language query)

-- fuga =
--   S.scotty 3000 $
--     S.get "/decode" $
--       (S.queryParam "data" :: S.ActionM T.Text) >>= \base64Url -> S.text $ case decodeBase64UriToJson $ TE.encodeUtf8 base64Url of
--         Left err -> TL.fromStrict $ T.pack err
--         Right decodedText -> TL.fromStrict decodedText

data Person = Person
  { name :: String,
    age :: Int
  }
  deriving (Show, Generic)

instance FromJSON Person
