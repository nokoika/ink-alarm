module SplaApi
  ( Root (..),
    Result (..),
    DefaultSchedule (..),
    Rule (..),
    Stage (..),
    EventMatch (..),
    EventSummary (..),
    fetchSchedule,
  )
where

import Control.Exception (SomeException, try)
import Data.Aeson ((.:), (.:?))
import qualified Data.Aeson as A (FromJSON (..), eitherDecode, withObject)
import qualified Data.ByteString.Lazy.Char8 as L8 (ByteString)
import GHC.Generics (Generic)
import Network.HTTP.Conduit (simpleHttp)
import Prelude (Bool, Either (..), Eq, IO, Int, Maybe, Show, String, ($), (<$>), (<*>))
import qualified Prelude as P (show, return)

newtype Root = Root
  { result :: Result
  }
  deriving (Show, Eq, Generic)

instance A.FromJSON Root where
  parseJSON = A.withObject "Root" $ \v ->
    Root
      <$> v
        .: "result"

data Result = Result
  { regular :: [DefaultSchedule],
    bankaraChallenge :: [DefaultSchedule],
    bankaraOpen :: [DefaultSchedule],
    x :: [DefaultSchedule],
    event :: [EventMatch]
  }
  deriving (Show, Eq, Generic)

instance A.FromJSON Result where
  parseJSON = A.withObject "Result" $ \v ->
    Result
      <$> v
        .: "regular"
      <*> v
        .: "bankara_challenge"
      <*> v
        .: "bankara_open"
      <*> v
        .: "x"
      <*> v
        .: "event"

data DefaultSchedule = DefaultSchedule
  { startTime :: String,
    endTime :: String,
    rule :: Maybe Rule, -- フェス中の場合は `Nothing`
    stages :: Maybe [Stage], -- フェス中の場合は `Nothing`
    isFest :: Bool
  }
  deriving (Show, Eq, Generic)

instance A.FromJSON DefaultSchedule where
  parseJSON = A.withObject "DefaultSchedule" $ \v ->
    DefaultSchedule
      <$> v
        .: "start_time"
      <*> v
        .: "end_time"
      <*> v
        .:? "rule"
      <*> v
        .:? "stages"
      <*> v
        .: "is_fest"

data Rule = Rule
  { key :: String,
    name :: String
  }
  deriving (Show, Eq, Generic)

instance A.FromJSON Rule where
  parseJSON = A.withObject "Rule" $ \v ->
    Rule
      <$> v
        .: "key"
      <*> v
        .: "name"

data Stage = Stage
  { id :: Int,
    name :: String,
    image :: String
  }
  deriving (Show, Eq, Generic)

instance A.FromJSON Stage where
  parseJSON = A.withObject "Stage" $ \v ->
    Stage
      <$> v
        .: "id"
      <*> v
        .: "name"
      <*> v
        .: "image"

data EventMatch = EventMatch
  { startTime :: String,
    endTime :: String,
    rule :: Rule,
    stages :: [Stage],
    eventSummary :: EventSummary,
    isFest :: Bool
  }
  deriving (Show, Eq, Generic)

instance A.FromJSON EventMatch where
  parseJSON = A.withObject "EventMatch" $ \v ->
    EventMatch
      <$> v
        .: "start_time"
      <*> v
        .: "end_time"
      <*> v
        .: "rule"
      <*> v
        .: "stages"
      <*> v
        .: "event"
      <*> v
        .: "is_fest"

data EventSummary = EventSummary
  { id :: String,
    name :: String,
    desc :: String
  }
  deriving (Show, Eq, Generic)

instance A.FromJSON EventSummary where
  parseJSON = A.withObject "EventSummary" $ \v ->
    EventSummary
      <$> v
        .: "id"
      <*> v
        .: "name"
      <*> v
        .: "desc"

fetchSchedule :: IO (Either String Root)
fetchSchedule = do
  let url = "https://spla3.yuu26.com/api/schedule"
  response <- try (simpleHttp url) :: IO (Either SomeException L8.ByteString)
  case response of
    Left err -> P.return $ Left $ P.show err
    Right body -> P.return $ A.eitherDecode body
