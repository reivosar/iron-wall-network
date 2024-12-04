{-# LANGUAGE OverloadedStrings #-}

module Infrastructure.Services.PostgresAuthService where

import Application.Auth.Services.AuthService
import qualified Application.Auth.Services.AuthUserDto as AuthUserDto
import qualified Application.Auth.Services.UserAccessTokenDto as UserAccessTokenDto
import qualified Application.Auth.Services.UserRefreshTokenDto as UserRefreshTokenDto
import Control.Monad.IO.Class (liftIO)
import Data.Text (Text)
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromRow (FromRow (..), field)
import Infrastructure.Database.Executor

instance FromRow AuthUserDto.AuthUserDto where
  fromRow =
    AuthUserDto.AuthUserDto
      <$> field
      <*> field
      <*> field
      <*> field

instance FromRow UserRefreshTokenDto.UserRefreshTokenDto where
  fromRow =
    UserRefreshTokenDto.UserRefreshTokenDto
      <$> field
      <*> field
      <*> field
      <*> field

instance FromRow UserAccessTokenDto.UserAccessTokenDto where
  fromRow =
    UserAccessTokenDto.UserAccessTokenDto
      <$> field
      <*> field
      <*> field
      <*> field

instance AuthService IO where
  findUserByUsername userName = do
    result <-
      liftIO $
        fetchOne
          "SELECT id, user_name, password_hash, auth_key_hash FROM system_users WHERE user_name = ?"
          [userName]

    case result of
      Left err -> pure $ Left (DatabaseError (show err))
      Right maybeUser -> pure $ Right maybeUser

  createAccessToken _ = return $ Left (TokenGenerationError "Not implemented")

  recreateAccessToken _ = return $ Left (TokenGenerationError "Not implemented")

  findAccessTokenByUserId userId = do
    result <-
      liftIO $
        fetchOne
          "SELECT user_id, access_token, issued_at, expires_at FROM user_access_tokens WHERE user_id = ?"
          [userId]

    case result of
      Left err -> pure $ Left (DatabaseError (show err))
      Right maybeToken -> pure $ Right maybeToken

  findRefreshTokenByRefreshToken refreshToken = do
    result <-
      liftIO $
        fetchOne
          "SELECT user_id, refresh_token, issued_at, expires_at FROM user_refresh_tokens WHERE user_id = ?"
          [refreshToken]

    case result of
      Left err -> pure $ Left (DatabaseError (show err))
      Right maybeToken -> pure $ Right maybeToken

  removeTokensByUserId _ = return $ Right ()

  invalidateTokensByUserId _ = return $ Right ()
