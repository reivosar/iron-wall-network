{-# LANGUAGE OverloadedStrings #-}

module Infrastructure.Services.PostgresAuthService
  ( findUserByUsername,
    createAccessToken,
    recreateAccessToken,
    invalidateToken,
  )
where

import Application.Auth.Services.AuthService
import qualified Application.Auth.Services.AuthUserDto as AuthUserDto
import qualified Application.Auth.Services.CreateAccessTokenResult as CreateAccessTokenResult
import qualified Application.Auth.Services.RecreateAccessTokenResult as RecreateAccessTokenResult
import qualified Application.Auth.Services.UserAccessTokenDto as UserAccessTokenDto
import qualified Application.Auth.Services.UserRefreshTokenDto as UserRefreshTokenDto
import Control.Monad.IO.Class (liftIO)
import Data.Text (Text, pack)
import qualified Data.Text as T
import Data.Time.Clock
  ( getCurrentTime,
  )
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromRow
  ( FromRow (..),
    field,
  )
import Infrastructure.Database.Executor
import Infrastructure.Services.TokenGenerator

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
      Left err -> pure $ Left (DatabaseError $ pack (show err))
      Right maybeUser -> do
        pure $ Right maybeUser

  createAccessToken userId = do
    transactionResult <- liftIO $
      withTransactionExecutor $ \conn -> do
        currentTime <- getCurrentTime
        accessTokenOutput <- generateAccessToken currentTime
        refreshTokenOutput <- generateRefreshToken currentTime
        _ <-
          execute
            conn
            "INSERT INTO user_access_token_logs (user_id, access_token, issued_at, expires_at, invalidated_at) \
            \SELECT user_id, access_token, issued_at, expires_at, CURRENT_TIMESTAMP \
            \FROM user_access_tokens WHERE user_id = ?"
            [userId]
        _ <-
          execute
            conn
            "INSERT INTO user_refresh_token_logs (user_id, refresh_token, issued_at, expires_at, invalidated_at) \
            \SELECT user_id, refresh_token, issued_at, expires_at, CURRENT_TIMESTAMP \
            \FROM user_refresh_tokens \
            \WHERE user_id = ?"
            [userId]
        _ <-
          execute
            conn
            "INSERT INTO user_access_tokens (user_id, access_token, issued_at, expires_at) \
            \VALUES (?, ?, ?, ?) \
            \ON CONFLICT (user_id) DO UPDATE \
            \SET access_token = EXCLUDED.access_token, \
            \    issued_at = EXCLUDED.issued_at, \
            \    expires_at = EXCLUDED.expires_at, \
            \    updated_at = CURRENT_TIMESTAMP"
            ( userId,
              accessToken accessTokenOutput,
              currentTime,
              accessTokenExpiresAt accessTokenOutput
            )
        _ <-
          execute
            conn
            "INSERT INTO user_refresh_tokens (user_id, refresh_token, issued_at, expires_at) \
            \VALUES (?, ?, ?, ?) \
            \ON CONFLICT (user_id) DO UPDATE \
            \SET refresh_token = EXCLUDED.refresh_token, \
            \    issued_at = EXCLUDED.issued_at, \
            \    expires_at = EXCLUDED.expires_at, \
            \    updated_at = CURRENT_TIMESTAMP"
            ( userId,
              refreshToken refreshTokenOutput,
              currentTime,
              refreshTokenExpiresAt refreshTokenOutput
            )

        pure $
          Right
            CreateAccessTokenResult.CreateAccessTokenResult
              { CreateAccessTokenResult.accessToken = accessToken accessTokenOutput,
                CreateAccessTokenResult.refreshToken = refreshToken refreshTokenOutput,
                CreateAccessTokenResult.issuedAt = currentTime,
                CreateAccessTokenResult.accessTokenExpiresAt = accessTokenExpiresAt accessTokenOutput,
                CreateAccessTokenResult.refreshTokenExpiresAt = refreshTokenExpiresAt refreshTokenOutput
              }

    case transactionResult of
      Left err -> pure $ Left (DatabaseError $ pack (show err))
      Right tokenResult -> pure $ Right tokenResult

  recreateAccessToken userId = do
    transactionResult <- liftIO $
      withTransactionExecutor $ \conn -> do
        currentTime <- getCurrentTime
        accessTokenOutput <- generateAccessToken currentTime
        _ <-
          execute
            conn
            "INSERT INTO user_access_token_logs (user_id, access_token, issued_at, expires_at, invalidated_at) \
            \SELECT user_id, access_token, issued_at, expires_at, CURRENT_TIMESTAMP \
            \FROM user_access_tokens WHERE user_id = ?"
            [userId]
        _ <-
          execute
            conn
            "INSERT INTO user_access_tokens (user_id, access_token, issued_at, expires_at) \
            \VALUES (?, ?, ?, ?) \
            \ON CONFLICT (user_id) DO UPDATE \
            \SET access_token = EXCLUDED.access_token, \
            \    issued_at = EXCLUDED.issued_at, \
            \    expires_at = EXCLUDED.expires_at, \
            \    updated_at = CURRENT_TIMESTAMP"
            ( userId,
              accessToken accessTokenOutput,
              currentTime,
              accessTokenExpiresAt accessTokenOutput
            )
        pure $
          Right
            RecreateAccessTokenResult.RecreateAccessTokenResult
              { RecreateAccessTokenResult.accessToken = accessToken accessTokenOutput,
                RecreateAccessTokenResult.issuedAt = currentTime,
                RecreateAccessTokenResult.accessTokenExpiresAt = accessTokenExpiresAt accessTokenOutput
              }
    case transactionResult of
      Left err -> pure $ Left (DatabaseError $ pack (show err))
      Right tokenResult -> pure $ Right tokenResult

  findAccessTokenByAccessToken token = do
    result <-
      liftIO $
        fetchOne
          "SELECT user_id, access_token, issued_at, expires_at FROM user_access_tokens WHERE access_token = ?"
          [token]

    case result of
      Left err -> pure $ Left (DatabaseError $ pack (show err))
      Right maybeToken -> pure $ Right maybeToken

  findRefreshTokenByRefreshToken rfrshTkn = do
    result <-
      liftIO $
        fetchOne
          "SELECT user_id, refresh_token, issued_at, expires_at FROM user_refresh_tokens WHERE refresh_token = ?"
          [rfrshTkn]

    case result of
      Left err -> pure $ Left (DatabaseError $ pack (show err))
      Right maybeToken -> pure $ Right maybeToken

  validateToken inputToken = do
    let tokenWithoutBearer = case T.stripPrefix "Bearer " inputToken of
          Just t -> T.strip t
          Nothing -> T.strip inputToken
    tokenResult <- findAccessTokenByAccessToken tokenWithoutBearer
    case tokenResult of
      Left err -> pure $ Left err
      Right Nothing -> pure $ Left (TokenNotFoundError "Access token not found")
      Right (Just token) -> do
        currentTime <- liftIO getCurrentTime
        let expiresAt = UserAccessTokenDto.expiresAt token

        if currentTime > expiresAt
          then pure $ Left (TokenExpiredError "Access token has expired")
          else pure $ Right ()

  invalidateToken inputToken = do
    let tokenWithoutBearer = case T.stripPrefix "Bearer " inputToken of
          Just t -> T.strip t
          Nothing -> T.strip inputToken

    tokenResult <- findAccessTokenByAccessToken tokenWithoutBearer

    case tokenResult of
      Left err -> pure $ Left err
      Right Nothing -> pure $ Left (TokenNotFoundError "Access token not found")
      Right (Just token) -> do
        let userId = UserAccessTokenDto.userId token

        transactionResult <- liftIO $
          withTransactionExecutor $ \conn -> do
            _ <-
              execute
                conn
                "INSERT INTO user_access_token_logs (user_id, access_token, issued_at, expires_at, invalidated_at) \
                \SELECT user_id, access_token, issued_at, expires_at, CURRENT_TIMESTAMP \
                \FROM user_access_tokens WHERE access_token = ?"
                [tokenWithoutBearer]
            _ <-
              execute
                conn
                "INSERT INTO user_refresh_token_logs (user_id, refresh_token, issued_at, expires_at, invalidated_at) \
                \SELECT user_id, refresh_token, issued_at, expires_at, CURRENT_TIMESTAMP \
                \FROM user_refresh_tokens \
                \WHERE user_id = (SELECT user_id FROM user_access_tokens WHERE access_token = ?)"
                [tokenWithoutBearer]
            _ <-
              execute
                conn
                "DELETE FROM user_access_tokens WHERE user_id = ?"
                [userId]
            _ <-
              execute
                conn
                "DELETE FROM user_refresh_tokens WHERE user_id = ?"
                [userId]

            pure (Right ())

        case transactionResult of
          Left err -> pure $ Left (DatabaseError $ pack (show err))
          Right () -> pure $ Right ()
