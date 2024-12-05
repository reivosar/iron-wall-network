{-# LANGUAGE OverloadedStrings #-}

module Middleware.EntryPointMiddleware (entryPointMiddleware) where

import Middleware.AuthMiddleware (authMiddleware)
import Middleware.LogMiddleware (logMiddleware)
import Network.Wai (Middleware)

entryPointMiddleware :: Middleware
entryPointMiddleware = logMiddleware . authMiddleware
