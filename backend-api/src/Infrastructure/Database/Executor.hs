{-# LANGUAGE OverloadedStrings #-}

module Infrastructure.Database.Executor where

import Control.Exception (SomeException, toException, try)
import qualified Data.ByteString.Char8 as BS
import Data.Int
import Data.Text (Text)
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.ToRow
import Database.PostgreSQL.Simple.Types
import Utils.Env
import Prelude (Either (..), IO, String, pure, userError)
import Prelude hiding (FilePath)

connectDb :: IO (Either SomeException Connection)
connectDb = do
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

fetchOne :: (FromRow r, ToRow q) => String -> q -> IO (Either SomeException (Maybe r))
fetchOne queryString params = do
  let queryStringBS = BS.pack queryString
  result <- connectDb
  case result of
    Left err -> pure (Left err)
    Right conn -> do
      queryResult <- try (query conn (Query queryStringBS) params)
      case queryResult of
        Left queryErr -> pure (Left queryErr)
        Right [row] -> pure (Right (Just row))
        Right [] -> pure (Right Nothing)
        Right _ -> pure $ Left (toException (userError "Unexpected result from query"))

fetchAll :: (FromRow r, ToRow q) => String -> q -> IO (Either SomeException [r])
fetchAll queryString params = do
  let queryStringBS = BS.pack queryString
  result <- connectDb
  case result of
    Left err -> pure (Left err)
    Right conn -> do
      queryResult <- try (query conn (Query queryStringBS) params)
      case queryResult of
        Left queryErr -> pure (Left queryErr)
        Right rows -> pure (Right rows)

withTransaction :: (Connection -> IO (Either SomeException a)) -> IO (Either SomeException a)
withTransaction action = do
  result <- connectDb
  case result of
    Left err -> pure (Left err)
    Right conn -> do
      beginResult <- try (execute_ conn "BEGIN TRANSACTION") :: IO (Either SomeException Int64)
      print beginResult
      actionResult <- action conn
      case actionResult of
        Right val -> do
          commitResult <- try (execute_ conn "COMMIT") :: IO (Either SomeException Int64)
          print commitResult
          pure (Right val)
        Left err -> do
          rollbackResult <- try (execute_ conn "ROLLBACK") :: IO (Either SomeException Int64)
          print rollbackResult
          pure (Left err)
