{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Infrastructure.Services.TokenGenerator
  ( AccessTokenOutput (..),
    RefreshTokenOutput (..),
    generateAccessToken,
    generateRefreshToken,
  )
where

import Data.Aeson (FromJSON, ToJSON)
import Data.Text (Text, pack)
import Data.Time.Clock (UTCTime, addUTCTime, getCurrentTime)
import Data.UUID (toText)
import GHC.Generics (Generic)
import System.Environment (getEnv)
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

generateAccessToken :: UTCTime -> IO AccessTokenOutput
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

generateRefreshToken :: UTCTime -> IO RefreshTokenOutput
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
