{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module Infrastructure.Services.Auth.TokenGenerator
  ( AccessTokenOutput (..),
    RefreshTokenOutput (..),
    generateAccessToken,
    generateRefreshToken,
  )
where

import Control.Monad.IO.Class (MonadIO)
import Data.Aeson (FromJSON, ToJSON)
import Data.Text (Text)
import Data.Time.Clock (UTCTime, addUTCTime)
import Data.UUID (toText)
import GHC.Generics (Generic)
import System.Environment ()
import Utils.Env
import Utils.HashGenerator (generateHMAC)
import Utils.UUIDGenerator (generateUUID)

data AccessTokenOutput = AccessTokenOutput
  { accessToken :: Text,
    accessTokenExpiresAt :: UTCTime
  }
  deriving (Show, Eq, Generic, FromJSON, ToJSON)

data RefreshTokenOutput = RefreshTokenOutput
  { refreshToken :: Text,
    refreshTokenExpiresAt :: UTCTime
  }
  deriving (Show, Eq, Generic, FromJSON, ToJSON)

generateAccessToken :: (MonadIO m) => UTCTime -> m AccessTokenOutput
generateAccessToken currentTime = do
  secretKey <- getEnvTextOrThrow "ACCESS_TOKEN_SECRET"
  let expiresAt = addUTCTime (60 * 60) currentTime
  uuid <- generateUUID
  let token = generateHMAC (toText uuid) secretKey
  return
    AccessTokenOutput
      { accessToken = token,
        accessTokenExpiresAt = expiresAt
      }

generateRefreshToken :: (MonadIO m) => UTCTime -> m RefreshTokenOutput
generateRefreshToken currentTime = do
  secretKey <- getEnvTextOrThrow "REFRESH_TOKEN_SECRET"
  let expiresAt = addUTCTime (7 * 24 * 60 * 60) currentTime
  uuid <- generateUUID
  let token = generateHMAC (toText uuid) secretKey
  return
    RefreshTokenOutput
      { refreshToken = token,
        refreshTokenExpiresAt = expiresAt
      }
