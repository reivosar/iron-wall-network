{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UndecidableInstances #-}

module Infrastructure.Events.RedisDomainEventPublisher
  ( DomainEventPublisherError (..),
    publishEvent,
  )
where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Aeson (Value, toJSON)
import qualified Data.ByteString.Char8 as BS
import Data.Text (Text, pack)
import qualified Data.Text as T
import Data.Time.Clock (getCurrentTime)
import Data.UUID (UUID, toString)
import Domain.DomainEventPublisher
import Domain.Event
import Infrastructure.Events.PostgresDomainEventStore
import Infrastructure.Redis.Executor

data DomainEventPublisherError
  = RedisConnectionError Text
  | RedisCommandError Text
  | EventStoreError Text
  deriving (Show, Eq)

instance (MonadIO m) => DomainEventPublisher m where
  publishEvent inputAggregateId inputAggregateType inputEventType inputTriggeredBy inputEventData inputMetadata = do
    result <- storeEvent inputAggregateId inputAggregateType inputEventType (Just inputTriggeredBy) (toJSON inputEventData) inputMetadata
    case result of
      Left errMsg -> return $ Left (UnexpectedError (pack $ show errMsg))
      Right eventId -> do
        let streamName = inputAggregateType <> "-events"
        sendResult <- liftIO $ withRedisTransaction $ \conn ->
          sendToRedis
            conn
            streamName
            [ ("eventId", BS.pack $ show eventId),
              ("aggregateId", BS.pack $ toString inputAggregateId),
              ("aggregateType", BS.pack $ T.unpack inputAggregateType),
              ("eventType", BS.pack $ T.unpack inputEventType)
            ]
        case sendResult of
          Left err -> return $ Left (PublishEventFailed (pack $ show err))
          Right _ -> return $ Right ()

storeEvent :: (MonadIO m) => UUID -> Text -> Text -> Maybe Text -> Value -> Maybe Value -> m (Either Text Int)
storeEvent inputAggregateId inputAggregateType currentEventType inputTriggeredBy inputEventData inputMetadata = do
  latestEventsResult <- getLatestEventsByAggregate (T.pack $ toString inputAggregateId) inputAggregateType
  case latestEventsResult of
    Left err -> return $ Left (pack $ "Failed to fetch latest events: " <> show err)
    Right [] -> do
      let initialSequenceNumber = 1
          initialVersion = 1
      persistNewEvent inputAggregateId inputAggregateType currentEventType inputTriggeredBy inputEventData inputMetadata initialSequenceNumber initialVersion
    Right latestEvents -> do
      let maxSequenceNumber = maximum $ map sequenceNumber latestEvents
          matchingEvent = filter (\e -> eventType e == currentEventType) latestEvents
          newVersion = case matchingEvent of
            [] -> 1
            matched -> version (head matched) + 1
          newSequenceNumber = maxSequenceNumber + 1
      checkAndPersistEvent inputAggregateId inputAggregateType currentEventType inputTriggeredBy inputEventData inputMetadata newSequenceNumber newVersion

persistNewEvent ::
  (MonadIO m) =>
  UUID ->
  Text ->
  Text ->
  Maybe Text ->
  Value ->
  Maybe Value ->
  Integer ->
  Integer ->
  m (Either Text Int)
persistNewEvent inputAggregateId inputAggregateType inputEventType inputTriggeredBy inputEventData inputMetadata inputSequenceNumber inputVersion = do
  currentTime <- liftIO getCurrentTime
  let eventToPersist =
        Event
          { aggregateId = T.pack $ toString inputAggregateId,
            aggregateType = inputAggregateType,
            eventType = inputEventType,
            eventData = inputEventData,
            sequenceNumber = inputSequenceNumber,
            version = inputVersion,
            triggeredBy = inputTriggeredBy,
            occurredAt = currentTime,
            metadata = inputMetadata
          }
  result <- persistEvent eventToPersist
  case result of
    Left err -> return $ Left (pack $ show err)
    Right eventId -> return $ Right eventId

checkAndPersistEvent ::
  (MonadIO m) =>
  UUID ->
  Text ->
  Text ->
  Maybe Text ->
  Value ->
  Maybe Value ->
  Integer ->
  Integer ->
  m (Either Text Int)
checkAndPersistEvent inputAggregateId inputAggregateType inputEventType inputTriggeredBy inputEventData inputMetadata inputSequenceNumber inputVersion = do
  nextEventResult <- getEventsByIdSinceSequenceNumber (T.pack $ toString inputAggregateId) inputAggregateType inputSequenceNumber
  case nextEventResult of
    Left err -> return $ Left (pack $ "Failed to fetch events since sequence number: " <> show err)
    Right nextEvents ->
      if not (null nextEvents)
        then return $ Left "Conflict detected: Another process has already persisted this event."
        else persistNewEvent inputAggregateId inputAggregateType inputEventType inputTriggeredBy inputEventData inputMetadata inputSequenceNumber inputVersion
