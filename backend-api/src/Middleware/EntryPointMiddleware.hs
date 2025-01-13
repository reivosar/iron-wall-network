{-# LANGUAGE OverloadedStrings #-}

module Middleware.EntryPointMiddleware (entryPointMiddleware) where

import Infrastructure.Services.Auth.PostgresAuditLogService ()
import Infrastructure.Services.Auth.PostgresAuthService ()
import Infrastructure.Services.Auth.PostgresOperatorIdService ()
import Middleware.AuthMiddleware (authMiddleware)
import Middleware.LogMiddleware (logMiddleware)
import Network.Wai (Middleware)

entryPointMiddleware :: Middleware
entryPointMiddleware = logMiddleware . authMiddleware
