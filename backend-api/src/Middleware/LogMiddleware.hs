{-# LANGUAGE OverloadedStrings #-}

module Middleware.LogMiddleware (logMiddleware) where

import Data.Text (pack)
import Data.Time (getCurrentTime)
import Database.PostgreSQL.Simple (Connection)
import Network.Wai (Middleware, rawPathInfo, requestHeaders, requestMethod, responseStatus)
import Service.AuditLogService (recordAuditLog)
import qualified Utils.Logger as Logger

logMiddleware :: Connection -> Middleware
logMiddleware conn app req sendResponse = do
  requestStartedAt <- getCurrentTime
  Logger.logInfo $
    "Request: " ++ show (requestMethod req) ++ " " ++ show (rawPathInfo req)

  let url = pack $ show $ rawPathInfo req
      method = pack $ show $ requestMethod req
      userId = extractUserIdFromHeaders req
      description = Nothing
      params = extractRequestParams req
      query = extractQueryString req
      ip = extractIpAddress req
  app req $ \response -> do
    requestEndedAt <- getCurrentTime
    let responseStatusCode = Just $ statusCode $ responseStatus response
        responseMessage = Nothing
    recordAuditLog conn url method userId description params query ip responseStatusCode responseMessage requestStartedAt

    Logger.logInfo $
      "Response: " ++ show (responseStatus response)
    sendResponse response
