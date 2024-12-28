{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Infrastructure.Events.PostgresDomainEventStore
  ( getLatestSnapshotById,
    getEventsByIdSinceSequenceNumber,
    persistEvent,
    persistEventAndSnapshot,
  )
where

import Control.Exception (SomeException, try)
import Control.Monad (void)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Except (ExceptT (..), runExceptT)
import Data.Aeson (Value)
import Data.Int (Int64)
import Data.Text (Text, pack)
import Data.Time.Clock (UTCTime)
import Database.PostgreSQL.Simple
import Domain.DomainEventStore
import Infrastructure.Database.Executor

-- Map exceptions to EventStoreError
mapExceptionToEventStoreError :: SomeException -> EventStoreError
mapExceptionToEventStoreError ex
  | "Optimistic lock" `elem` words (show ex) = OptimisticLockError (pack $ show ex)
  | "Serialization" `elem` words (show ex) = SerializationError (pack $ show ex)
  | "Deserialization" `elem` words (show ex) = DeserializationError (pack $ show ex)
  | otherwise = IOError (pack $ show ex)

-- Convert a database row to an Event
rowToEvent :: (Text, Text, Text, Text, Text, Value, Integer, Maybe Text, UTCTime, Maybe Value) -> Event
rowToEvent (partitionKey, sortKey, aggregateId, aggregateType, eventTypeCol, eventDataCol, sequenceNumber, triggeredBy, occurredAt, metadata) =
  Event
    { eventPartitionKey = partitionKey,
      eventSortKey = sortKey,
      eventAggregateId = aggregateId,
      eventAggregateType = aggregateType,
      eventType = eventTypeCol,
      eventData = eventDataCol,
      eventSequenceNumber = sequenceNumber,
      eventTriggeredBy = triggeredBy,
      eventOccurredAt = occurredAt,
      eventMetadata = metadata
    }

-- Convert a database row to a Snapshot
rowToSnapshot :: (Text, Text, Text, Text, Integer, Integer, Value) -> Snapshot
rowToSnapshot (partitionKey, sortKey, aggregateId, aggregateType, sequenceNumber, version, snapshotDataCol) =
  Snapshot
    { snapshotPartitionKey = partitionKey,
      snapshotSortKey = sortKey,
      snapshotAggregateId = aggregateId,
      snapshotAggregateType = aggregateType,
      snapshotSequenceNumber = sequenceNumber,
      snapshotVersion = version,
      snapshotData = snapshotDataCol
    }

-- Implement the DomainEventStore interface
instance DomainEventStore IO where
  -- GetLatestSnapshotById
  getLatestSnapshotById aggrgtId = do
    result <-
      liftIO $
        fetchOne
          "SELECT partition_key, sort_key, aggregate_id, aggregate_type, sequence_number, version, snapshot_data \
          \FROM event_snapshots WHERE aggregate_id = ? ORDER BY sequence_number DESC LIMIT 1"
          [aggrgtId]

    case result of
      Left err -> pure $ Left err
      Right Nothing -> pure $ Right Nothing
      Right (Just row) -> pure $ Right (Just $ rowToSnapshot row)

  -- GetEventsByIdSinceSequenceNumber
  getEventsByIdSinceSequenceNumber aggrgtId seqNr = do
    result <-
      liftIO $
        fetchAll
          "SELECT partition_key, sort_key, aggregate_id, aggregate_type, event_type, event_data, sequence_number, triggered_by, occurred_at, metadata \
          \FROM events WHERE aggregate_id = ? AND sequence_number >= ? ORDER BY sequence_number"
          (aggrgtId, seqNr)

    case result of
      Left err -> pure $ Left err
      Right rows -> pure $ Right (map rowToEvent rows)

  -- PersistEvent
  persistEvent event = do
    transactionResult <- liftIO $ withTransactionExecutor $ \conn -> runExceptT $ do
      eventId <- ExceptT $ tryInsertEvent conn event
      ExceptT $ tryInsertPendingEvents conn eventId
      pure eventId
    case transactionResult of
      Left err -> pure $ Left (mapExceptionToEventStoreError err)
      Right eventId -> pure $ Right (fromIntegral eventId :: Int)

  -- PersistEventAndSnapshot
  persistEventAndSnapshot event snapshot = do
    transactionResult <- liftIO $ withTransactionExecutor $ \conn -> runExceptT $ do
      eventId <- ExceptT $ tryInsertEvent conn event
      ExceptT $ tryInsertSnapshot conn snapshot
      ExceptT $ tryInsertPendingEvents conn eventId
      pure eventId
    case transactionResult of
      Left err -> pure $ Left (mapExceptionToEventStoreError err)
      Right eventId -> pure $ Right (fromIntegral eventId :: Int)

tryInsertEvent :: Connection -> Event -> IO (Either SomeException Int64)
tryInsertEvent conn event = do
  try $ do
    [Only eventId] <-
      query
        conn
        "INSERT INTO events (partition_key, sort_key, aggregate_id, aggregate_type, event_type, event_data, sequence_number, triggered_by, occurred_at, metadata) \
        \VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?) RETURNING event_id"
        ( eventPartitionKey event,
          eventSortKey event,
          eventAggregateId event,
          eventAggregateType event,
          eventType event,
          eventData event,
          eventSequenceNumber event,
          eventTriggeredBy event,
          eventOccurredAt event,
          eventMetadata event
        )
    return eventId

tryInsertSnapshot :: Connection -> Snapshot -> IO (Either SomeException ())
tryInsertSnapshot conn snapshot = do
  try $ do
    void $
      execute
        conn
        "INSERT INTO event_snapshots (partition_key, sort_key, aggregate_id, aggregate_type, sequence_number, version, snapshot_data) \
        \VALUES (?, ?, ?, ?, ?, ?, ?)"
        ( snapshotPartitionKey snapshot,
          snapshotSortKey snapshot,
          snapshotAggregateId snapshot,
          snapshotAggregateType snapshot,
          snapshotSequenceNumber snapshot,
          snapshotVersion snapshot,
          snapshotData snapshot
        )

tryInsertPendingEvents :: Connection -> Int64 -> IO (Either SomeException ())
tryInsertPendingEvents conn newEventId = do
  try $ do
    void $
      execute
        conn
        "INSERT INTO pending_events (event_id) VALUES (?)"
        (Only newEventId)
