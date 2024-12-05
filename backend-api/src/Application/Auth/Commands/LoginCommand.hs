{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Application.Auth.Commands.LoginCommand where

import Data.Aeson (FromJSON, ToJSON)
import Data.Text (Text)
import Data.Time (UTCTime)
import GHC.Generics (Generic)

data LoginRequest = LoginRequest
  { userName :: Text,
    password :: Text,
    authKey :: Text
  }
  deriving (Show, Generic, FromJSON, ToJSON)

data TokenResponse = TokenResponse
  { accessToken :: Text,
    refreshToken :: Text,
    issuedAt :: UTCTime,
    accessTokenExpiresAt :: UTCTime,
    refreshTokenExpiresAt :: UTCTime
  }
  deriving (Show, Generic, FromJSON, ToJSON)
