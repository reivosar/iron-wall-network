{-# LANGUAGE OverloadedStrings #-}

module Infrastructure.Repositories.PostgresAuditLogRepository where

import Control.Exception (SomeException, toException, try)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Text (Text)
import Data.Time (UTCTime)
import Database.PostgreSQL.Simple (Connection, Only (..), Query, execute)
import Infrastructure.Database.Executor (fetchOne, withTransactionExecutor)
import Middleware.AuditLogRepository (AuditLog (..), AuditLogRepository (..))

instance AuditLogRepository IO where
  generateId = do
    result <- fetchOne "SELECT nextval('audit_logs_id_seq')" ()
    case result of
      Left err -> pure $ Left err
      Right Nothing -> pure $ Left (toException (userError "Failed to fetch a new ID"))
      Right (Just (Only newId)) -> pure $ Right newId

  save logEntry = do
    withTransactionExecutor $ \conn -> do
      let query :: Query
          query =
            "INSERT INTO audit_logs \
            \(id, url, method, user_id, description, parameters, query, ip_address, response_status, response_message, request_started_at, request_ended_at) \
            \VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)"
      let params =
            ( auditLogId logEntry,
              url logEntry,
              method logEntry,
              userId logEntry,
              description logEntry,
              parameters logEntry,
              Middleware.AuditLogRepository.query logEntry,
              ipAddress logEntry,
              responseStatus logEntry,
              responseMessage logEntry,
              requestStartedAt logEntry,
              requestEndedAt logEntry
            )
      result <- try $ execute conn query params
      case result of
        Left err -> pure $ Left err
        Right _ -> pure $ Right ()
