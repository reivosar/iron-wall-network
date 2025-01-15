{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UndecidableInstances #-}

module Infrastructure.Services.BankAccount.RedisUsernameUniquenessValidator
  ( UsernameUniquenessValidator (..),
  )
where

import Control.Monad.IO.Class (MonadIO, liftIO)
import qualified Data.ByteString.Char8 as BS
import Data.Text (Text, pack, unpack)
import Database.Redis
import Domain.BankAccount.Services.UsernameUniquenessValidator
import Domain.BankAccount.ValueObject.Username (Username, unwrapUsername)
import Domain.Error (DomainError, mkDomainError)
import GHC.Exception (SomeException, toException)
import Infrastructure.Redis.Executor (withRedisTransaction)

groupPrefixedKey :: Text -> Text -> Text -> Text
groupPrefixedKey group category key = group <> ":" <> category <> ":" <> key

constructRedisKey :: Username -> BS.ByteString
constructRedisKey username =
  BS.pack . unpack $ groupPrefixedKey "account" "username" (unwrapUsername username)

instance (MonadIO m) => UsernameUniquenessValidator m where
  validateUsernameUniqueness username = liftIO $ do
    let redisKey = constructRedisKey username
    result <- withRedisTransaction $ \conn -> do
      checkResult <- checkRedisKey conn redisKey
      case checkResult of
        Left err -> return $ Left err
        Right _ -> setRedisKey conn redisKey
    case result of
      Left err -> return $ Left $ mkDomainError $ pack (show err)
      Right _ -> return $ Right ()

checkRedisKey :: Connection -> BS.ByteString -> IO (Either SomeException ())
checkRedisKey conn redisKey = do
  redisCheck <- runRedis conn $ get redisKey
  case redisCheck of
    Left redisErr ->
      return $ Left $ toException (userError ("Redis error while checking username: " <> show redisErr))
    Right (Just _) ->
      return $ Left $ toException (userError "Username already exists.")
    Right Nothing ->
      return $ Right ()

setRedisKey :: Connection -> BS.ByteString -> IO (Either SomeException ())
setRedisKey conn redisKey = do
  redisSet <- runRedis conn $ set redisKey "1"
  case redisSet of
    Left redisErr ->
      return $ Left $ toException (userError ("Failed to add username to Redis: " <> show redisErr))
    Right _ ->
      return $ Right ()
