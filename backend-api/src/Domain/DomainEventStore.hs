{-# LANGUAGE DeriveGeneric #-}

module Domain.DomainEventStore
  ( DomainEventStore (..),
    Event,
    EventStoreError (..),
  )
where

import Control.Exception (SomeException)
import Data.Text (Text)
import Domain.Event (Event)

data EventStoreError
  = OptimisticLockError Text
  | SerializationError Text
  | DeserializationError Text
  | IOError Text
  deriving (Show, Eq)

class (Monad m) => DomainEventStore m where
  getLatestEventsByAggregate :: Text -> Text -> m (Either SomeException [Event])
  getEventsByIdSinceSequenceNumber :: Text -> Text -> Integer -> m (Either SomeException [Event])
  persistEvent :: Event -> m (Either EventStoreError Int)
