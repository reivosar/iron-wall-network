{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}

module Infrastructure.Events.PostgresDomainEventStore
  ( getLatestEventsByAggregate,
    getEventsByIdSinceSequenceNumber,
    persistEvent,
  )
where

import Control.Exception (SomeException, try)
import Control.Monad (void)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Trans.Except (ExceptT (..), runExceptT)
import Data.Aeson (Value)
import Data.Int (Int64)
import Data.Text (Text, concat, intercalate, pack)
import Data.Time.Clock (UTCTime)
import Database.PostgreSQL.Simple
import Domain.DomainEventStore
import Domain.Event
import Infrastructure.Database.Executor

-- Map exceptions to EventStoreError
mapExceptionToEventStoreError :: SomeException -> EventStoreError
mapExceptionToEventStoreError ex
  | "Optimistic lock" `elem` words (show ex) = OptimisticLockError (pack $ show ex)
  | "Serialization" `elem` words (show ex) = SerializationError (pack $ show ex)
  | "Deserialization" `elem` words (show ex) = DeserializationError (pack $ show ex)
  | otherwise = IOError (pack $ show ex)

-- Convert a database row to EventRetrieved
rowToEvent :: (Text, Text, Text, Value, Integer, Integer, Maybe Text, UTCTime, Maybe Value) -> Event
rowToEvent (aId, aType, eType, eData, seqNum, ver, trigBy, occAt, meta) =
  Event
    { aggregateId = aId,
      aggregateType = aType,
      eventType = eType,
      eventData = eData,
      sequenceNumber = seqNum,
      version = ver,
      triggeredBy = trigBy,
      occurredAt = occAt,
      metadata = meta
    }

generatePlaceholders :: Int -> Text
generatePlaceholders count = "(" <> intercalate ", " (customReplicate count "?") <> ")"

customReplicate :: Int -> Text -> [Text]
customReplicate count str = replicate count str

-- Implement the DomainEventStore interface
instance (Applicative m, MonadIO m) => DomainEventStore m where
  -- GetLatestEventByAggregate
  getLatestEventsByAggregate aggrgtId aggrgtType = do
    result <-
      liftIO $
        fetchAll
          "SELECT e.aggregate_id, e.aggregate_type, e.event_type, e.event_data, \
          \e.sequence_number, e.version, e.triggered_by, e.occurred_at, e.metadata \
          \FROM events e \
          \WHERE e.aggregate_id = ? AND e.aggregate_type = ? \
          \AND e.sequence_number = ( \
          \  SELECT MAX(sequence_number) \
          \  FROM events \
          \  WHERE aggregate_id = ? AND aggregate_type = ? \
          \)"
          (aggrgtId, aggrgtType, aggrgtId, aggrgtType)

    case result of
      Left err -> pure $ Left err
      Right rows -> pure $ Right (map rowToEvent rows)

  getEventsByAggregateAndEventNames aggrgtType eventNames = do
    let placeholders = generatePlaceholders (length eventNames)
        sql =
          "SELECT e.aggregate_id, e.aggregate_type, e.event_type, e.event_data, \
          \e.sequence_number, e.version, e.triggered_by, e.occurred_at, e.metadata \
          \FROM events e \
          \WHERE e.aggregate_type = ? AND e.event_type IN "
            <> placeholders
            <> " \
               \AND e.sequence_number = ( \
               \  SELECT MAX(sequence_number) \
               \  FROM events \
               \  WHERE aggregate_id = e.aggregate_id AND aggregate_type = e.aggregate_type \
               \)"

    result <- liftIO $ fetchAll sql (aggrgtType : eventNames)
    case result of
      Left err -> pure $ Left err
      Right rows -> pure $ Right (map rowToEvent rows)

  -- GetEventsByIdSinceSequenceNumber
  getEventsByIdSinceSequenceNumber aggrgtId aggrgtType seqNr = do
    result <-
      liftIO $
        fetchAll
          "SELECT e.aggregate_id, e.aggregate_type, e.event_type, e.event_data, \
          \e.sequence_number, e.version, e.triggered_by, e.occurred_at, e.metadata \
          \FROM events e \
          \WHERE e.aggregate_id = ? AND e.aggregate_type = ? AND e.sequence_number > ? \
          \ORDER BY e.sequence_number"
          (aggrgtId, aggrgtType, seqNr)

    case result of
      Left err -> pure $ Left err
      Right rows -> pure $ Right (map rowToEvent rows)

  -- PersistEvent
  persistEvent eventToPersist = do
    transactionResult <- liftIO $ withTransactionExecutor $ \conn -> runExceptT $ do
      eventId <- ExceptT $ tryInsertEvent conn eventToPersist
      ExceptT $ tryUpdateLatestPointer conn eventId eventToPersist
      ExceptT $ tryInsertPendingEvents conn eventId
      pure eventId
    case transactionResult of
      Left err -> pure $ Left (mapExceptionToEventStoreError err)
      Right eventId -> pure $ Right (fromIntegral eventId :: Int)

tryInsertEvent :: Connection -> Event -> IO (Either SomeException Int64)
tryInsertEvent conn eventToPersist = do
  try $ do
    [Only eventId] <-
      query
        conn
        "INSERT INTO events (aggregate_id, aggregate_type, event_type, event_data, sequence_number, version, triggered_by, occurred_at, metadata) \
        \VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?) RETURNING event_id"
        ( aggregateId eventToPersist,
          aggregateType eventToPersist,
          eventType eventToPersist,
          eventData eventToPersist,
          sequenceNumber eventToPersist,
          version eventToPersist,
          triggeredBy eventToPersist,
          occurredAt eventToPersist,
          metadata eventToPersist
        )
    return eventId

tryUpdateLatestPointer :: Connection -> Int64 -> Event -> IO (Either SomeException ())
tryUpdateLatestPointer conn newEventId eventToPersist = do
  try $ do
    void $
      execute
        conn
        "INSERT INTO latest_event_pointers (aggregate_id, aggregate_type, event_type, last_event_id, last_sequence_number) \
        \VALUES (?, ?, ?, ?, ?) \
        \ON CONFLICT (aggregate_id, aggregate_type, event_type) DO UPDATE SET \
        \last_event_id = EXCLUDED.last_event_id, last_sequence_number = EXCLUDED.last_sequence_number, updated_at = NOW()"
        ( aggregateId eventToPersist,
          aggregateType eventToPersist,
          eventType eventToPersist,
          newEventId,
          sequenceNumber eventToPersist
        )

tryInsertPendingEvents :: Connection -> Int64 -> IO (Either SomeException ())
tryInsertPendingEvents conn newEventId = do
  try $ do
    void $
      execute
        conn
        "INSERT INTO pending_events (event_id) VALUES (?)"
        (Only newEventId)
