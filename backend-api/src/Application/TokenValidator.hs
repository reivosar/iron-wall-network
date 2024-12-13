{-# LANGUAGE OverloadedStrings #-}

module Application.TokenValidator
  ( mapAuthServiceErrorToTokenInvalidationError,
    validateToken,
  )
where

import Application.Auth.Services.AuthService (AuthService)
import qualified Application.Auth.Services.AuthService as AuthService
import Application.TokenInvalidationError
  ( TokenInvalidationError (..),
    createExpiredTokenError,
    createInvalidTokenError,
    createUnknownError,
  )
import Data.Text (Text)
import Infrastructure.Services.PostgresAuthService

validateToken :: (AuthService m, Monad m) => Text -> m (Either TokenInvalidationError ())
validateToken token = do
  result <- AuthService.validateToken token
  case result of
    Left err -> do
      return $ Left (mapAuthServiceErrorToTokenInvalidationError err)
    Right () -> do
      return $ Right ()

mapAuthServiceErrorToTokenInvalidationError :: AuthService.AuthServiceError -> TokenInvalidationError
mapAuthServiceErrorToTokenInvalidationError err = case err of
  AuthService.TokenNotFoundError msg -> createInvalidTokenError msg
  AuthService.TokenExpiredError msg -> createExpiredTokenError msg
  AuthService.DatabaseError msg -> createUnknownError msg
  _ -> createUnknownError "Unexpected error occurred during token validation"
