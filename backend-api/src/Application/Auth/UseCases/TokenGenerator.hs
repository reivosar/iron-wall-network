{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Application.Auth.UseCases.TokenGenerator where

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

generateAccessToken :: IO AccessTokenOutput
generateAccessToken = do
  secretKey <- getEnvTextOrThrow "ACCESS_TOKEN_SECRET"
  currentTime <- getCurrentTime
  let expiresIn = addUTCTime (60 * 60) currentTime
  uuid <- generateUUID
  let token = generateHMAC (toText uuid) secretKey
  return
    AccessTokenOutput
      { accessToken = token,
        accessTokenExpiresAt = expiresIn
      }

generateRefreshToken :: IO RefreshTokenOutput
generateRefreshToken = do
  secretKey <- getEnvTextOrThrow "REFRESH_TOKEN_SECRET"
  currentTime <- getCurrentTime
  let expiresIn = addUTCTime (7 * 24 * 60 * 60) currentTime
  uuid <- generateUUID
  let token = generateHMAC (toText uuid) secretKey
  return
    RefreshTokenOutput
      { refreshToken = token,
        refreshTokenExpiresAt = expiresIn
      }
