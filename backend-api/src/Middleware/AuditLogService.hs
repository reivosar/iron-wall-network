module Middleware.AuditLogService (AuditLog (..), AuditLogService (..)) where

import Control.Exception (SomeException)
import Data.Text (Text)
import Data.Time (UTCTime)
import Data.UUID (UUID)

data AuditLog = AuditLog
  { auditLogId :: Int,
    transactionId :: UUID,
    operatorId :: Maybe Int,
    ipAddress :: Maybe Text,
    userAgent :: Maybe Text,
    description :: Maybe Text,
    url :: Text,
    contentType :: Maybe Text,
    method :: Text,
    parameters :: Maybe Text,
    queryText :: Maybe Text,
    responseStatusCode :: Maybe Int,
    responseMessage :: Maybe Text,
    requestStartedAt :: UTCTime,
    requestEndedAt :: Maybe UTCTime
  }

class AuditLogService m where
  generateId :: m (Either SomeException (Int))
  save :: AuditLog -> m (Either SomeException ())
