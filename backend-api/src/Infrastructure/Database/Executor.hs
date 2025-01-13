{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Infrastructure.Database.Executor (connectDb, fetchAll, fetchOne, withTransactionExecutor) where

import Control.Exception
  ( SomeException,
    toException,
    try,
  )
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Int
import Data.Text (Text)
import qualified Data.Text.Encoding as TE
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.ToRow ()
import Database.PostgreSQL.Simple.Types
import Utils.Env
import Prelude hiding (FilePath)

connectDb :: (MonadIO m) => m (Either SomeException Connection)
connectDb = liftIO $ do
  dbNameResult <- getEnvString "BACKEND_DB_NAME"
  userResult <- getEnvString "BACKEND_DB_USER"
  passwordResult <- getEnvString "BACKEND_DB_PASSWORD"
  hostResult <- getEnvString "BACKEND_DB_HOST"

  case (dbNameResult, userResult, passwordResult, hostResult) of
    (Right dbName, Right user, Right password, Right host) -> do
      conn <-
        connect
          defaultConnectInfo
            { connectDatabase = dbName,
              connectUser = user,
              connectPassword = password,
              connectHost = host
            }
      pure (Right conn)
    _ -> pure (Left (toException (userError "Failed to read environment variables")))

fetchOne :: (MonadIO m, FromRow r, ToRow q) => Text -> q -> m (Either SomeException (Maybe r))
fetchOne queryText params = do
  result <- connectDb
  case result of
    Left err -> pure (Left err)
    Right conn -> liftIO $ do
      let queryByteString = TE.encodeUtf8 queryText
      queryResult <- try (query conn (Query queryByteString) params)
      case queryResult of
        Left queryErr -> pure (Left queryErr)
        Right [row] -> pure (Right (Just row))
        Right [] -> pure (Right Nothing)
        Right _ -> pure $ Left (toException (userError "Unexpected result from query"))

fetchAll :: (MonadIO m, FromRow r, ToRow q) => Text -> q -> m (Either SomeException [r])
fetchAll queryText params = do
  result <- connectDb
  case result of
    Left err -> pure (Left err)
    Right conn -> liftIO $ do
      let queryByteString = TE.encodeUtf8 queryText
      queryResult <- try (query conn (Query queryByteString) params)
      case queryResult of
        Left queryErr -> pure (Left queryErr)
        Right rows -> pure (Right rows)

withTransactionExecutor :: (MonadIO m) => (Connection -> m (Either SomeException a)) -> m (Either SomeException a)
withTransactionExecutor action = do
  result <- connectDb
  case result of
    Left err -> pure (Left err)
    Right conn -> do
      -- BEGIN TRANSACTION
      beginResult <- liftIO $ (try (execute_ conn "BEGIN TRANSACTION") :: IO (Either SomeException Int64))
      liftIO $ printResult "BEGIN TRANSACTION" beginResult

      -- Execute action
      actionResult <- action conn

      -- COMMIT or ROLLBACK
      case actionResult of
        Right val -> do
          commitResult <- liftIO $ (try (execute_ conn "COMMIT") :: IO (Either SomeException Int64))
          liftIO $ printResult "COMMIT" commitResult
          pure (Right val)
        Left err -> do
          rollbackResult <- liftIO $ (try (execute_ conn "ROLLBACK") :: IO (Either SomeException Int64))
          liftIO $ printResult "ROLLBACK" rollbackResult
          pure (Left err)

printResult :: (Show a) => String -> Either SomeException a -> IO ()
printResult label result = putStrLn $ label ++ ": " ++ show result
