module Query
  ( FilterCondition (..),
    QueryRoot (..),
    UtcOffsetTimeZone (..),
    StageFilter (..),
    TimeSlot (..),
    TimeSlotTimeOfDay (..),
    TimeSlotDayOfWeek (..),
    Language (..),
    Mode (..),
    Rule (..),
    parseBase64UrlRaw,
    parseBase64UrlGzip,
    convertTimeSlotToTimeOfDayRange,
  )
where

import qualified Codec.Compression.GZip as GZ
import qualified Data.Aeson as A
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base64.URL as BU
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.Char8 as L8
import qualified Data.Text as Text
import qualified Data.Text.Encoding as TE
import qualified Data.Time as Time
import qualified Data.Time.Calendar as C
import qualified Data.Time.LocalTime as LT
import qualified Date as D
import GHC.Generics (Generic)
import Prelude (Applicative (pure), Bool, Bounded, Either (Left, Right), Enum, Eq, Int, Maybe (Just, Nothing), Show, String, fail, show, ($), (++), (.))

data QueryRoot = QueryRoot
  { language :: Language,
    utcOffset :: UtcOffsetTimeZone,
    filters :: [FilterCondition]
  }
  deriving (Show, Eq, Generic)

instance A.FromJSON QueryRoot

-- 言語
data Language
  = Japanese
  | English
  deriving (Show, Eq, Generic, Enum, Bounded)

instance A.FromJSON Language where
  parseJSON = A.withText "Language" $ \case
    "ja" -> pure Japanese
    "en" -> pure English
    _invalid -> pure English

-- UTCオフセット
newtype UtcOffsetTimeZone = UtcOffsetTimeZone {timeZone :: Time.TimeZone}
  deriving (Show, Eq, Generic)

instance A.FromJSON UtcOffsetTimeZone where
  parseJSON = A.withText "UtcOffsetTimeZone" $ \t -> case D.timeZoneFromOffsetString (Text.unpack t) of
    Just timeZone -> pure $ UtcOffsetTimeZone timeZone
    Nothing -> fail $ "Invalid UtcOffsetTimeZone: " ++ show t

-- mode (オープン, チャレンジ, X, レギュラー, イベント)
data Mode
  = BankaraOpen
  | BankaraChallenge
  | XMatch
  | Regular
  | Event
  deriving (Show, Eq, Generic, Enum, Bounded)

instance A.FromJSON Mode where
  parseJSON = A.withText "Mode" $ \t -> case t of
    "bankara_open" -> pure BankaraOpen
    "bankara_challenge" -> pure BankaraChallenge
    "x" -> pure XMatch
    "regular" -> pure Regular
    "event" -> pure Event
    _invalid -> fail $ "Invalid Mode: " ++ show t

-- ルール(ガチエリア, ガチヤグラ, ガチホコ, ガチアサリ, ナワバリ)
data Rule
  = SplatZones
  | TowerControl
  | Rainmaker
  | ClamBlitz
  | TurfWar
  deriving (Show, Eq, Generic, Enum, Bounded)

instance A.FromJSON Rule where
  parseJSON = A.withText "Rule" $ \t -> case t of
    "area" -> pure SplatZones
    "yagura" -> pure TowerControl
    "hoko" -> pure Rainmaker
    "asari" -> pure ClamBlitz
    "nawabari" -> pure TurfWar
    _invalid -> fail $ "Invalid Rule: " ++ show t

-- フィルタ条件
data FilterCondition = FilterCondition
  { modes :: Maybe [Mode],
    stages :: Maybe StageFilter,
    rules :: Maybe [Rule],
    timeSlots :: Maybe [TimeSlot]
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

newtype TimeSlotDayOfWeek = TimeSlotDayOfWeek {dayOfWeek :: C.DayOfWeek}
  deriving (Eq, Show)

-- FromJSON インスタンスの実装
instance A.FromJSON TimeSlotDayOfWeek where
  parseJSON = A.withText "TimeSlotDayOfWeek" $ \t ->
    case Text.toLower t of
      "mon" -> pure $ TimeSlotDayOfWeek C.Monday
      "tue" -> pure $ TimeSlotDayOfWeek C.Tuesday
      "wed" -> pure $ TimeSlotDayOfWeek C.Wednesday
      "thu" -> pure $ TimeSlotDayOfWeek C.Thursday
      "fri" -> pure $ TimeSlotDayOfWeek C.Friday
      "sat" -> pure $ TimeSlotDayOfWeek C.Saturday
      "sun" -> pure $ TimeSlotDayOfWeek C.Sunday
      other -> fail $ "Invalid TimeSlotDayOfWeek: " ++ show other

newtype TimeSlotTimeOfDay = TimeSlotTimeOfDay {timeOfDay :: LT.TimeOfDay}
  deriving (Eq, Show)

instance A.FromJSON TimeSlotTimeOfDay where
  parseJSON = A.withText "TimeSlotTimeOfDay" $ \t -> case D.timeOfDayFromString (Text.unpack t) of
    Just timeOfDay -> pure $ TimeSlotTimeOfDay timeOfDay
    Nothing -> fail $ "Invalid TimeSlotTimeOfDay: " ++ show t

-- 時間帯
data TimeSlot = TimeSlot
  { start :: TimeSlotTimeOfDay, -- HH:mm
    end :: TimeSlotTimeOfDay, -- HH:mm
    dayOfWeeks :: Maybe [TimeSlotDayOfWeek]
  }
  deriving (Show, Eq, Generic)

instance A.FromJSON TimeSlot

convertTimeSlotToTimeOfDayRange :: TimeSlot -> (LT.TimeOfDay, LT.TimeOfDay)
convertTimeSlotToTimeOfDayRange TimeSlot {start = TimeSlotTimeOfDay start', end = TimeSlotTimeOfDay end'} =
  (start', end')

parseJsonToQueryRoot :: L8.ByteString -> Either String QueryRoot
parseJsonToQueryRoot json = A.eitherDecode json :: Either String QueryRoot

decodeBase64UriToJson :: BS.ByteString -> Either String BS.ByteString
decodeBase64UriToJson base64Url' =
  case BU.decode base64Url' of
    Left err -> Left $ "Base64 decode error: " ++ show err
    Right decoded -> Right decoded

parseBase64UrlRaw :: Text.Text -> Either String QueryRoot
parseBase64UrlRaw base64Url = do
  decodedText <- decodeBase64UriToJson (TE.encodeUtf8 base64Url)
  (parseJsonToQueryRoot . BL.fromStrict) decodedText

parseBase64UrlGzip :: Text.Text -> Either String QueryRoot
parseBase64UrlGzip base64Url = do
  decodedText <- decodeBase64UriToJson (TE.encodeUtf8 base64Url)
  -- 本当は gzip の解凍に失敗した場合にエラーハンドリングしたい気がするが、
  -- zlib の関数が Either を返してくれない・・・のでぱっとできない
  (parseJsonToQueryRoot . GZ.decompress . BL.fromStrict) decodedText
