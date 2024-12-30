{-# LANGUAGE OverloadedStrings #-}

module Middleware.EntryPointMiddleware (entryPointMiddleware) where

import Infrastructure.Services.PostgresAuditLogService ()
import Infrastructure.Services.PostgresAuthService ()
import Infrastructure.Services.PostgresOperatorIdService ()
import Middleware.AuthMiddleware (authMiddleware)
import Middleware.LogMiddleware (logMiddleware)
import Network.Wai (Middleware)

entryPointMiddleware :: Middleware
entryPointMiddleware = logMiddleware . authMiddleware
