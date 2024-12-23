{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Application.Auth.Commands.RefreshTokenCommand
  ( RefreshTokenRequest (..),
    RefreshTokenResponse (..),
  )
where

import Data.Aeson (FromJSON, ToJSON)
import Data.Text (Text)
import Data.Time (UTCTime)
import GHC.Generics (Generic)

data RefreshTokenRequest = RefreshTokenRequest
  { refreshToken :: Text
  }
  deriving (Show, Generic, FromJSON, ToJSON)

data RefreshTokenResponse = RefreshTokenResponse
  { accessToken :: Text,
    issuedAt :: UTCTime,
    expiresAt :: UTCTime
  }
  deriving (Show, Generic, FromJSON, ToJSON)
