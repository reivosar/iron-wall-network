{-# LANGUAGE OverloadedStrings #-}

module Infrastructure.Repositories.PostgresAuditLogRepository
  ( generateId,
    save,
  )
where

import Control.Exception
  ( toException,
    try,
  )
import Control.Monad.IO.Class ()
import Data.Text ()
import Data.Time ()
import Data.UUID ()
import Database.PostgreSQL.Simple
  ( Only (..),
    Query,
    execute,
  )
import Infrastructure.Database.Executor
  ( fetchOne,
    withTransactionExecutor,
  )
import Middleware.AuditLogRepository
  ( AuditLog (..),
    AuditLogRepository (..),
  )

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
            \(id, transaction_id, operator_id, ip_address, user_agent, description, url, content_type, method, parameters, query, response_status, response_message, request_started_at, request_ended_at) \
            \VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)"

      let params =
            ( auditLogId logEntry,
              transactionId logEntry,
              operatorId logEntry,
              ipAddress logEntry,
              userAgent logEntry,
              description logEntry,
              url logEntry,
              contentType logEntry,
              method logEntry,
              parameters logEntry,
              queryText logEntry,
              responseStatusCode logEntry,
              responseMessage logEntry,
              requestStartedAt logEntry,
              requestEndedAt logEntry
            )
      result <- try $ execute conn query params
      case result of
        Left err -> pure $ Left err
        Right _ -> pure $ Right ()
