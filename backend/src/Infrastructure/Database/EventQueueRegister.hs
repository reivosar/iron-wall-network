{-# LANGUAGE OverloadedStrings #-}

module Infrastructure.Database.EventQueueRegister where

import Database.PostgreSQL.Simple
import Data.Aeson (encode, Value) 
import Data.UUID (UUID)
import Data.Time.Clock (getCurrentTime)
import Data.Maybe (fromMaybe) 
import Infrastructure.Database.DbUtils (connectDb)
import Prelude (IO, Either(..), Maybe, String, putStrLn, show, (++), return)

storeEventAndSnapshot :: UUID -> String -> String -> String -> Value -> Maybe Value -> IO ()
storeEventAndSnapshot aggregateId aggregateType eventType triggeredBy eventData metadata = do
    result <- connectDb
    case result of
        Left err -> putStrLn ("Error connecting to database: " ++ show err)
        Right conn -> do
            currentTime <- getCurrentTime
            let metadataJson = encode (fromMaybe "" metadata)

            _ <- execute conn
                "INSERT INTO events (aggregate_id, aggregate_type, event_type, event_data, event_triggered_by, event_timestamp, metadata) VALUES (?, ?, ?, ?, ?, ?, ?)"
                (aggregateId, aggregateType, eventType, eventData, triggeredBy, currentTime, metadataJson)

            let snapshotData = eventData 
            _ <- execute conn
                "INSERT INTO event_snapshots (aggregate_id, aggregate_type, last_event_id, snapshot_data, snapshot_timestamp) VALUES (?, ?, (SELECT MAX(event_id) FROM events), ?, ?)"
                (aggregateId, aggregateType, snapshotData, currentTime)

            return () 
