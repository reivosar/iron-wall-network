{-# LANGUAGE OverloadedStrings #-}

module Infrastructure.Events.RedisDomainEventPublisher
  ( DomainEventPublisherError (..),
    publishEvent,
  )
where

import Control.Exception (SomeException, try)
import Data.Aeson (toJSON)
import qualified Data.ByteString.Char8 as BS
import Data.Text (Text, pack)
import qualified Data.Text as T
import Data.UUID (UUID, toString)
import Database.Redis
import Domain.DomainEventPublisher
import Infrastructure.Events.PostgresEventQueueStore (storeEventAndSnapshot)
import Utils.Env

data DomainEventPublisherError
  = RedisConnectionError Text
  | RedisCommandError Text
  | EventStoreError Text
  deriving (Show, Eq)

redisConnect :: IO (Either DomainEventPublisherError Connection)
redisConnect = do
  hostResult <- getEnvString "MESSAGE_BROKER_HOST"
  portResult <- getEnvString "MESSAGE_BROKER_PORT"

  case (hostResult, portResult) of
    (Right host, Right portStr) -> do
      let port = read portStr :: Int
      connResult <- try $ connect (defaultConnectInfo {connectHost = host, connectPort = PortNumber (fromIntegral port)}) :: IO (Either SomeException Connection)
      case connResult of
        Right conn -> return $ Right conn
        Left err -> return $ Left (RedisConnectionError (pack $ "Failed to connect to Redis: " <> show err))
    (Left e, _) -> return $ Left (RedisConnectionError (pack $ "Failed to get host: " <> show e))
    (_, Left e) -> return $ Left (RedisConnectionError (pack $ "Failed to get port: " <> show e))

sendMessageToRedis :: Text -> Int -> UUID -> Text -> Text -> IO (Either DomainEventPublisherError ())
sendMessageToRedis streamName eventId aggregateId aggregateType eventType = do
  connResult <- redisConnect
  case connResult of
    Left err -> return $ Left err
    Right conn -> do
      result <- runRedis conn $ do
        let keyValuePairs =
              [ ("eventId", BS.pack $ show eventId),
                ("aggregateId", BS.pack $ toString aggregateId),
                ("aggregateType", BS.pack $ T.unpack aggregateType),
                ("eventType", BS.pack $ T.unpack eventType)
              ]
        xadd (BS.pack $ T.unpack streamName) "*" keyValuePairs
      case result of
        Left err -> return $ Left (RedisCommandError (pack $ "Redis command failed: " ++ show err))
        Right _ -> return $ Right ()

instance DomainEventPublisher IO where
  publishEvent aggregateId aggregateType eventType triggeredBy eventData metadata = do
    result <- storeEventAndSnapshot aggregateId aggregateType eventType triggeredBy (toJSON eventData) metadata
    case result of
      Left errMsg -> return $ Left (UnexpectedError (pack $ show errMsg))
      Right eventId -> do
        let streamName = aggregateType <> "-events"
        sendResult <- sendMessageToRedis streamName eventId aggregateId aggregateType eventType
        case sendResult of
          Left (RedisConnectionError msg) -> return $ Left (PublishEventFailed msg)
          Left (RedisCommandError msg) -> return $ Left (PublishEventFailed msg)
          Left (EventStoreError msg) -> return $ Left (PublishEventFailed msg)
          Right _ -> return $ Right ()
