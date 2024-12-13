{-# LANGUAGE OverloadedStrings #-}

module Infrastructure.Events.RedisDomainEventPublisher
  ( DomainEventPublisherError (..),
  )
where

import Control.Exception
  ( SomeException,
    try,
  )
import Data.Aeson
  ( ToJSON,
    Value,
    toJSON,
  )
import qualified Data.ByteString.Char8 as BS
import Data.Text (Text)
import qualified Data.Text as T
import Data.UUID (UUID, toString)
import Database.Redis
import Domain.DomainEventPublisher
import Infrastructure.Events.PostgresEventQueueStore (storeEventAndSnapshot)
import Utils.Env
import Utils.Env (getEnvString)
import Prelude
  ( Either (..),
    Eq,
    IO,
    Int,
    Maybe (..),
    Show,
    String,
    fromIntegral,
    read,
    return,
    show,
    ($),
    (++),
  )

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
        Left err -> return $ Left (RedisConnectionError (T.pack $ "Failed to connect to Redis: " ++ show err))
    (Left e, _) -> return $ Left (RedisConnectionError (T.pack $ "Failed to get host: " ++ show e))
    (_, Left e) -> return $ Left (RedisConnectionError (T.pack $ "Failed to get port: " ++ show e))

sendMessageToRedis :: String -> Int -> UUID -> String -> String -> IO (Either DomainEventPublisherError ())
sendMessageToRedis streamName eventId aggregateId aggregateType eventType = do
  connResult <- redisConnect
  case connResult of
    Left err -> return $ Left err
    Right conn -> do
      result <- runRedis conn $ do
        let streamNameBS = BS.pack streamName
            eventIdBS = BS.pack (show eventId)
            aggregateIdBS = BS.pack (toString aggregateId)
            aggregateTypeBS = BS.pack aggregateType
            eventTypeBS = BS.pack eventType
        let keyValuePairs =
              [ ("eventId", eventIdBS),
                ("aggregateId", aggregateIdBS),
                ("aggregateType", aggregateTypeBS),
                ("eventType", eventTypeBS)
              ]
        xadd streamNameBS "*" keyValuePairs
      case result of
        Left err -> return $ Left (RedisCommandError (T.pack $ "Redis command failed: " ++ show err))
        Right _ -> return $ Right ()

instance DomainEventPublisher IO where
  publishEvent aggregateId aggregateType eventType triggeredBy eventData metadata = do
    result <- storeEventAndSnapshot aggregateId aggregateType eventType triggeredBy (toJSON eventData) metadata
    case result of
      Left errMsg -> return $ Left (UnexpectedError (show errMsg))
      Right eventId -> do
        let streamName = aggregateType ++ "-events"
        sendResult <- sendMessageToRedis streamName eventId aggregateId aggregateType eventType
        case sendResult of
          Left (RedisConnectionError msg) -> return $ Left (PublishEventFailed (T.unpack msg))
          Left (RedisCommandError msg) -> return $ Left (PublishEventFailed (T.unpack msg))
          Left (EventStoreError msg) -> return $ Left (PublishEventFailed (T.unpack msg))
          Right _ -> return $ Right ()
