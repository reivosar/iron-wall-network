{-# LANGUAGE OverloadedStrings #-}

module Infrastructure.Events.RedisDomainEventPublisher
  ( DomainEventPublisherError (..),
    publishEvent,
  )
where

import Data.Aeson (toJSON)
import qualified Data.ByteString.Char8 as BS
import Data.Text (Text, pack)
import qualified Data.Text as T
import Data.UUID (toString)
import Domain.DomainEventPublisher
import Infrastructure.Events.PostgresEventQueueStore (storeEventAndSnapshot)
import Infrastructure.Redis.Executor

data DomainEventPublisherError
  = RedisConnectionError Text
  | RedisCommandError Text
  | EventStoreError Text
  deriving (Show, Eq)

instance DomainEventPublisher IO where
  publishEvent aggregateId aggregateType eventType triggeredBy eventData metadata = do
    result <- storeEventAndSnapshot aggregateId aggregateType eventType triggeredBy (toJSON eventData) metadata
    case result of
      Left errMsg -> return $ Left (UnexpectedError (pack $ show errMsg))
      Right eventId -> do
        let streamName = aggregateType <> "-events"
        sendResult <- withRedisTransaction $ \conn ->
          sendToRedis
            conn
            streamName
            [ ("eventId", BS.pack $ show eventId),
              ("aggregateId", BS.pack $ toString aggregateId),
              ("aggregateType", BS.pack $ T.unpack aggregateType),
              ("eventType", BS.pack $ T.unpack eventType)
            ]
        case sendResult of
          Left err -> return $ Left (PublishEventFailed (pack $ show err))
          Right _ -> return $ Right ()
