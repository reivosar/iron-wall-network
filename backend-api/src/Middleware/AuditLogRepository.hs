module Middleware.AuditLogRepository where

import Control.Exception (SomeException)
import Data.Text (Text)
import Data.Time (UTCTime)

data AuditLog = AuditLog
  { auditLogId :: Int,
    url :: Text,
    method :: Text,
    userId :: Maybe Int,
    description :: Maybe Text,
    parameters :: Maybe Text,
    query :: Maybe Text,
    ipAddress :: Maybe Text,
    responseStatus :: Maybe Int,
    responseMessage :: Maybe Text,
    requestStartedAt :: UTCTime,
    requestEndedAt :: UTCTime
  }

class AuditLogRepository m where
  generateId :: m (Either SomeException (Int))
  save :: AuditLog -> m (Either SomeException ())
