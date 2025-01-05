{-# LANGUAGE OverloadedStrings #-}

module Infrastructure.Redis.Executor
  ( sendToRedis,
    withRedisTransaction,
  )
where

import Control.Exception (SomeException, toException, try)
import qualified Data.ByteString.Char8 as BS
import Data.Text (Text)
import qualified Data.Text as T
import Database.Redis
import Utils.Env

redisConnect :: IO (Either SomeException Connection)
redisConnect = do
  hostResult <- getEnvString "MESSAGE_BROKER_HOST"
  portResult <- getEnvString "MESSAGE_BROKER_PORT"
  passwordResult <- getEnvString "MESSAGE_BROKER_PASSWORD"

  case (hostResult, portResult, passwordResult) of
    (Right host, Right portStr, Right password) -> do
      let port = read portStr :: Int
      let connectInfo =
            defaultConnectInfo
              { connectHost = host,
                connectPort = PortNumber (fromIntegral port),
                connectAuth = Just (BS.pack password)
              }
      connResult <- try $ connect connectInfo :: IO (Either SomeException Connection)
      return connResult
    _ -> return $ Left (toException (userError "Failed to read Redis environment variables"))

sendToRedis :: Connection -> Text -> [(BS.ByteString, BS.ByteString)] -> IO (Either SomeException ())
sendToRedis conn streamName keyValuePairs = do
  result <- runRedis conn $ xadd (BS.pack $ T.unpack streamName) "*" keyValuePairs
  case result of
    Left err -> return $ Left (toException (userError $ "Redis command failed: " ++ show err))
    Right _ -> return $ Right ()

withRedisTransaction :: (Connection -> IO (Either SomeException a)) -> IO (Either SomeException a)
withRedisTransaction action = do
  connectionResult <- redisConnect
  case connectionResult of
    Left err -> pure (Left err)
    Right conn -> action conn
