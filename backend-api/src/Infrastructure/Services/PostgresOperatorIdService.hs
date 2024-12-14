{-# LANGUAGE OverloadedStrings #-}

module Infrastructure.Services.PostgresOperatorIdService
  ( getOperatorIdByUserName,
    getOperatorIdByAccessToken,
    getOperatorIdByRefreshToken,
  )
where

import Control.Exception (SomeException)
import Database.PostgreSQL.Simple (Only (..))
import Infrastructure.Database.Executor
  ( fetchOne,
  )
import Middleware.OperatorIdService (OperatorIdService (..))

convertToInt :: Either SomeException (Maybe (Only Int)) -> Either SomeException (Maybe Int)
convertToInt = fmap (fmap fromOnly)

instance OperatorIdService IO where
  getOperatorIdByUserName input = do
    result <- fetchOne "SELECT id FROM system_users WHERE user_name = ?" [input]
    pure $ convertToInt result

  getOperatorIdByAccessToken input = do
    result <- fetchOne "SELECT user_id FROM user_access_tokens WHERE access_token = ?" [input]
    pure $ convertToInt result

  getOperatorIdByRefreshToken input = do
    result <- fetchOne "SELECT user_id FROM user_refresh_tokens WHERE refresh_token = ?" [input]
    pure $ convertToInt result
