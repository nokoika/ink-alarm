module SplaApi
  ( Root (..),
    Result (..),
    DefaultSchedule (..),
    Rule (..),
    Stage (..),
    EventMatch (..),
    EventSummary (..),
  )
where

import Data.Aeson ((.:), (.:?))
import qualified Data.Aeson as A (FromJSON (..), withObject)
import GHC.Generics (Generic)
import Prelude as P (Bool, Eq, Int, Maybe, Show, String, ($), (<$>), (<*>))

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
    event :: EventSummary,
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
