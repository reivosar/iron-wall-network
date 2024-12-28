{-# LANGUAGE DeriveGeneric #-}

module Domain.DomainEventStore
  ( DomainEventStore (..),
    Event (..),
    Snapshot (..),
    EventStoreError (..),
  )
where

import Control.Exception (SomeException)
import Data.Aeson (Value)
import Data.Text (Text)
import Data.Time.Clock (UTCTime)
import GHC.Generics (Generic)

data Event = Event
  { eventPartitionKey :: Text,
    eventSortKey :: Text,
    eventAggregateId :: Text,
    eventAggregateType :: Text,
    eventType :: Text,
    eventData :: Value,
    eventSequenceNumber :: Integer,
    eventTriggeredBy :: Maybe Text,
    eventOccurredAt :: UTCTime,
    eventMetadata :: Maybe Value
  }
  deriving (Show, Generic)

data Snapshot = Snapshot
  { snapshotPartitionKey :: Text,
    snapshotSortKey :: Text,
    snapshotAggregateId :: Text,
    snapshotAggregateType :: Text,
    snapshotSequenceNumber :: Integer,
    snapshotVersion :: Integer,
    snapshotData :: Value
  }
  deriving (Show, Generic)

data EventStoreError
  = OptimisticLockError Text
  | SerializationError Text
  | DeserializationError Text
  | IOError Text
  deriving (Show, Eq)

class (Monad m) => DomainEventStore m where
  getLatestSnapshotById :: Text -> m (Either SomeException (Maybe Snapshot))
  getEventsByIdSinceSequenceNumber :: Text -> Integer -> m (Either SomeException [Event])
  persistEvent :: Event -> m (Either EventStoreError Int)
  persistEventAndSnapshot :: Event -> Snapshot -> m (Either EventStoreError Int)
