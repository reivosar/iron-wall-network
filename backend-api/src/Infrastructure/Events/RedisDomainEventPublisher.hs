{-# LANGUAGE OverloadedStrings #-}

module Infrastructure.Events.RedisDomainEventPublisher where

import Data.Aeson (ToJSON, toJSON, encode, Value)
import Database.Redis
import Infrastructure.Database.EventQueueRegister (storeEventAndSnapshot)
import qualified Data.ByteString.Lazy.Char8 as LBS
import qualified Data.ByteString.Char8 as BS
import Data.UUID (UUID, toString)
import System.Environment (getEnv)
import Control.Exception (try, SomeException)
import Prelude (IO, Either(..), Maybe(..), String, Int, read, show, return, (++), error, fromIntegral, ($))

redisConnect :: IO Connection
redisConnect = do
    hostResult <- try (getEnv "MESSAGE_BROKER_HOST") :: IO (Either SomeException String)
    portResult <- try (getEnv "MESSAGE_BROKER_PORT") :: IO (Either SomeException String)

    case (hostResult, portResult) of
        (Right host, Right portStr) -> do
            let port = read portStr :: Int
            conn <- connect (defaultConnectInfo { connectHost = host, connectPort = PortNumber (fromIntegral port) })
            return conn
        (Left e, _) -> error ("Failed to get host: " ++ show e)
        (_, Left e) -> error ("Failed to get port: " ++ show e)

sendMessageToRedis :: ToJSON a => String -> Int -> a -> IO ()
sendMessageToRedis streamName eventId msg = do
    let encodedMsg = encode msg
    conn <- redisConnect
    result <- runRedis conn $ do
        let streamNameBS = BS.pack streamName
            eventIdBS = BS.pack (show eventId)
            encodedMsgStrict = LBS.toStrict encodedMsg
        let keyValuePairs = [("eventId", eventIdBS), ("message", encodedMsgStrict)]
        xadd streamNameBS "*" keyValuePairs
        
    case result of
        Left err -> BS.putStrLn $ "Error: " `BS.append` BS.pack (show err) 
        Right _  -> BS.putStrLn "Message sent successfully" 

publishEvent :: (ToJSON a) => UUID -> String -> String -> String -> a -> Maybe Value -> IO ()
publishEvent aggregateId aggregateType eventType triggeredBy eventData metadata = do
    result <- storeEventAndSnapshot aggregateId aggregateType eventType triggeredBy (toJSON eventData) metadata
    case result of
        Left errMsg -> BS.putStrLn (BS.pack ("Failed to store event: " ++ errMsg))
        Right eventId -> do
            let streamName = aggregateType ++ "-events"
            sendMessageToRedis streamName eventId eventData
            return ()
