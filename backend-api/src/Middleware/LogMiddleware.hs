{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Middleware.LogMiddleware (logMiddleware) where

import Control.Monad.IO.Class (liftIO)
import Data.Maybe (fromMaybe)
import Data.Text (pack, stripPrefix)
import Data.Text.Encoding (decodeUtf8)
import Data.Time.Clock (getCurrentTime)
import Middleware.AuditLogService
import Middleware.LogMiddlewareHelpers
import Middleware.OperatorIdService
import Network.HTTP.Types (statusCode)
import Network.Wai
import qualified Utils.Logger as Logger
import Utils.UUIDGenerator

logMiddleware :: (OperatorIdService IO, AuditLogService IO) => Middleware
logMiddleware app originalRequest sendResponse = do
  startTime <- liftIO getCurrentTime
  transactionUUID <- liftIO generateUUID
  generateResult <- generateId
  (req, bodyChunks) <- cacheRequestBody originalRequest
  reqestParameters <- encodeParameters bodyChunks

  let query = encodeQuery (queryString req)
      ipAddressFromHeader = fmap decodeUtf8 (getClientIP req)
      userAgentFromHeader = fmap decodeUtf8 (lookup "User-Agent" (requestHeaders req))
      contentTypeFromHeader = fmap decodeUtf8 (lookup "Content-Type" (requestHeaders req))
      authHeader = fmap (stripBearer . decodeUtf8) (lookup "Authorization" (requestHeaders req))
        where
          stripBearer token = fromMaybe token (stripPrefix "Bearer " token)
      urlFromRequest = decodeUtf8 (rawPathInfo req)
      methodFromRequest = pack (show (requestMethod req))

  case generateResult of
    Left err -> do
      Logger.logError $
        "[TraceID: N/A] Failed to generate ID: " ++ show err
      app req sendResponse
    Right logId -> do
      Logger.logInfo $
        "[TraceID: "
          ++ show logId
          ++ "] Request: "
          ++ show (requestMethod req)
          ++ " "
          ++ show (rawPathInfo req)
          ++ ", Body: "
          ++ show reqestParameters
          ++ ", Query: "
          ++ show query

      app req $ \response -> do
        endTime <- liftIO getCurrentTime
        extractedResponseMessage <- extractResponseMessage response
        operatorIdResult <- getOperatorId urlFromRequest reqestParameters authHeader
        let maybeOperatorId = either (const Nothing) id operatorIdResult

        let auditLog =
              AuditLog
                { auditLogId = logId,
                  transactionId = transactionUUID,
                  operatorId = maybeOperatorId,
                  ipAddress = ipAddressFromHeader,
                  userAgent = userAgentFromHeader,
                  description = Nothing,
                  url = urlFromRequest,
                  contentType = contentTypeFromHeader,
                  method = methodFromRequest,
                  parameters = reqestParameters,
                  queryText = query,
                  responseStatusCode = Just (statusCode (responseStatus response)),
                  responseMessage = Just $ extractedResponseMessage,
                  requestStartedAt = startTime,
                  requestEndedAt = Just endTime
                }

        Logger.logInfo $ "[TraceID: " ++ show logId ++ "] Response: " ++ show (responseStatus response)

        saveResult <- save auditLog
        case saveResult of
          Left saveErr ->
            Logger.logError $
              "[TraceID: " ++ show logId ++ "] Failed to save audit log: " ++ show saveErr
          Right _ -> return ()

        sendResponse response
