{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Middleware.AuthMiddleware (authMiddleware) where

import Application.Auth.Services.AuthService (AuthService)
import Application.TokenInvalidationError
import Application.TokenValidator (validateToken)
import Data.ByteString ()
import Data.Text (isPrefixOf, unpack)
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8)
import Network.HTTP.Types.Status (unauthorized401)
import Network.Wai
  ( Middleware,
    rawPathInfo,
    requestHeaders,
    responseLBS,
  )
import qualified Utils.Logger as Logger

authMiddleware :: (AuthService IO) => Middleware
authMiddleware app req sendResponse = do
  let path = dropApiVersionPrefix $ decodeUtf8 (rawPathInfo req)
      isExcluded = any (`isPrefixOf` path) ["/auth/login", "/auth/refresh", "/auth/logout"]
  if isExcluded
    then app req sendResponse
    else do
      let authHeader = lookup "Authorization" (requestHeaders req)
      case authHeader of
        Just token -> do
          let tokenText = T.stripPrefix "Bearer " (decodeUtf8 token)
          case tokenText of
            Just actualToken -> do
              validationResult <- validateToken actualToken
              case validationResult of
                Right () -> app req sendResponse
                Left err -> do
                  Logger.logError $ "Token validation failed: " ++ formatTokenInvalidationError err
                  sendResponse unauthorizedResponse
            Nothing -> do
              Logger.logError "Invalid Authorization header format"
              sendResponse unauthorizedResponse
        Nothing -> do
          Logger.logError "Missing Authorization header"
          sendResponse unauthorizedResponse
  where
    unauthorizedResponse =
      responseLBS unauthorized401 [("Content-Type", "text/plain")] "Unauthorized"

formatTokenInvalidationError :: TokenInvalidationError -> String
formatTokenInvalidationError (InvalidToken msg) = "Invalid token: " ++ unpack msg
formatTokenInvalidationError (ExpiredToken msg) = "Expired token: " ++ unpack msg
formatTokenInvalidationError (UnknownError msg) = "Unknown error: " ++ unpack msg

dropApiVersionPrefix :: T.Text -> T.Text
dropApiVersionPrefix path =
  case T.stripPrefix "/" path of
    Just stripped ->
      case T.splitOn "/" stripped of
        (_ : rest) -> "/" <> T.intercalate "/" rest
        _ -> path
    Nothing -> path
