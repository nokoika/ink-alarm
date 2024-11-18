module Query (FilterCondition (..), NotificationSetting (..), QueryRoot (..), StageFilter (..), TimeSlot (..), parseBase64Url) where

import qualified Data.Aeson as A (FromJSON (..), eitherDecode)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base64.URL as BU (decode)
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.Char8 as L8
import qualified Data.Text as T (Text)
import qualified Data.Text.Encoding as TE (decodeUtf8', encodeUtf8)
import GHC.Generics (Generic)
import Prelude (Bool, Either (Left, Right), Eq, Int, Maybe, Show, String, ($))
import qualified Prelude as P (show)

data QueryRoot = QueryRoot
  { language :: String, -- "ja" | "en"
    utcOffset :: String, -- e.g., "+09:00"
    filters :: [FilterCondition]
  }
  deriving (Show, Eq, Generic)

instance A.FromJSON QueryRoot

-- TODO: matchType とかを enum にする

-- フィルタ条件
data FilterCondition
  = FilterCondition
  { matchType :: String, -- "bankara_open" | "bankara_challenge" | "x" | "regular" | "event"
    stages :: Maybe StageFilter,
    rules :: Maybe [String], -- ["TURF_WAR", "AREA", ...]
    timeSlots :: Maybe [TimeSlot],
    notifications :: Maybe [NotificationSetting]
  }
  deriving (Show, Eq, Generic)

instance A.FromJSON FilterCondition

-- ステージフィルタ
data StageFilter = StageFilter
  { matchBothStages :: Bool,
    stageIds :: [Int]
  }
  deriving (Show, Eq, Generic)

instance A.FromJSON StageFilter

-- 時間帯
data TimeSlot = TimeSlot
  { start :: String, -- HH:mm
    end :: String, -- HH:mm
    dayOfWeek :: Maybe String -- e.g., "Monday"
  }
  deriving (Show, Eq, Generic)

instance A.FromJSON TimeSlot

-- 通知設定
newtype NotificationSetting = NotificationSetting
  { minutesBefore :: Int
  -- TODO: 毎日何時、みたいな設定もできるようにする
  }
  deriving (Show, Eq, Generic)

instance A.FromJSON NotificationSetting

parseBase64Url :: T.Text -> Either String QueryRoot
parseBase64Url base64Url = case decodeBase64UriToJson $ TE.encodeUtf8 base64Url of
  Left err -> Left err
  Right decodedText -> parseJsonToQueryRoot $ BL.fromStrict $ TE.encodeUtf8 decodedText
  where
    decodeBase64UriToJson :: BS.ByteString -> Either String T.Text
    decodeBase64UriToJson base64Url' = case BU.decode base64Url' of
      Left err -> Left $ P.show err
      Right decoded -> case TE.decodeUtf8' decoded of
        Left err -> Left $ P.show err
        Right decodedText -> Right decodedText

    parseJsonToQueryRoot :: L8.ByteString -> Either String QueryRoot
    parseJsonToQueryRoot json = A.eitherDecode json :: Either String QueryRoot
