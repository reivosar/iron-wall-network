{-# LANGUAGE OverloadedStrings #-}

module Middleware.LogMiddleware (logMiddleware) where

import Network.Wai (Middleware, rawPathInfo, requestMethod, responseStatus)
import qualified Utils.Logger as Logger

logMiddleware :: Middleware
logMiddleware app req sendResponse = do
  Logger.logInfo $
    "Request: " ++ show (requestMethod req) ++ " " ++ show (rawPathInfo req)
  app req $ \response -> do
    Logger.logInfo $
      "Response: " ++ show (responseStatus response)
    sendResponse response
