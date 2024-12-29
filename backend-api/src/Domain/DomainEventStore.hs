{-# LANGUAGE DeriveGeneric #-}

module Domain.DomainEventStore
  ( DomainEventStore (..),
    Event (..),
    EventStoreError (..),
  )
where

import Control.Exception (SomeException)
import Data.Aeson (Value)
import Data.Text (Text)
import Data.Time.Clock (UTCTime)
import GHC.Generics (Generic)

data Event = Event
  { aggregateId :: Text,
    aggregateType :: Text,
    eventType :: Text,
    eventData :: Value,
    sequenceNumber :: Integer,
    version :: Integer,
    triggeredBy :: Maybe Text,
    occurredAt :: UTCTime,
    metadata :: Maybe Value
  }
  deriving (Show, Generic)

data EventStoreError
  = OptimisticLockError Text
  | SerializationError Text
  | DeserializationError Text
  | IOError Text
  deriving (Show, Eq)

class (Monad m) => DomainEventStore m where
  getLatestEventByAggregate :: Text -> Text -> m (Either SomeException (Maybe Event))
  getEventsByIdSinceSequenceNumber :: Text -> Text -> Integer -> m (Either SomeException [Event])
  persistEvent :: Event -> m (Either EventStoreError Int)
