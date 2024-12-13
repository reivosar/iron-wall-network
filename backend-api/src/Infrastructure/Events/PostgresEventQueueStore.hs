{-# LANGUAGE OverloadedStrings #-}

module Infrastructure.Events.PostgresEventQueueStore (storeEventAndSnapshot) where

import Data.Aeson (Value, encode)
import Data.Maybe (fromMaybe)
import Data.Time.Clock (getCurrentTime)
import Data.UUID (UUID)
import Database.PostgreSQL.Simple
import Infrastructure.Database.Executor (connectDb)
import Prelude
  ( Either (..),
    Eq (..),
    IO,
    Int,
    Maybe,
    String,
    putStrLn,
    return,
    show,
    (++),
  )

storeEventAndSnapshot :: UUID -> String -> String -> String -> Value -> Maybe Value -> IO (Either String Int)
storeEventAndSnapshot aggregateId aggregateType eventType triggeredBy eventData metadata = do
  result <- connectDb
  case result of
    Left err -> do
      putStrLn ("Error connecting to database: " ++ show err)
      return (Left "Error connecting to database")
    Right conn -> do
      currentTime <- getCurrentTime
      let metadataJson = encode (fromMaybe "" metadata)

      [Only eventId] <-
        query
          conn
          "INSERT INTO events (aggregate_id, aggregate_type, event_type, event_data, event_triggered_by, event_timestamp, metadata) \
          \VALUES (?, ?, ?, ?, ?, ?, ?) RETURNING event_id"
          (aggregateId, aggregateType, eventType, eventData, triggeredBy, currentTime, metadataJson)

      snapshotResult <-
        query
          conn
          "SELECT last_event_id FROM event_snapshots WHERE aggregate_id = ? ORDER BY snapshot_timestamp DESC LIMIT 1"
          (Only aggregateId)

      case snapshotResult of
        [Only lastEventId] -> do
          if (eventId :: Int) /= (lastEventId :: Int)
            then
              let snapshotData = eventData
               in do
                    _ <-
                      execute
                        conn
                        "INSERT INTO event_snapshots (aggregate_id, aggregate_type, last_event_id, snapshot_data, snapshot_timestamp) \
                        \VALUES (?, ?, ?, ?, ?)"
                        (aggregateId, aggregateType, (eventId :: Int), snapshotData, currentTime)
                    return ()
            else putStrLn "No need to update snapshot, events are already covered."
        _ -> putStrLn "No previous snapshot found, proceeding with new event."

      _ <-
        execute
          conn
          "INSERT INTO pending_events (event_id) VALUES (?)"
          (Only eventId)

      return (Right eventId)
