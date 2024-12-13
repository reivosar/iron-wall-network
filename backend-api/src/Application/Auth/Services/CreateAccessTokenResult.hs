{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Application.Auth.Services.CreateAccessTokenResult (CreateAccessTokenResult (..)) where

import Data.Aeson (FromJSON, ToJSON)
import Data.Text (Text)
import Data.Time (UTCTime)
import GHC.Generics (Generic)

data CreateAccessTokenResult = CreateAccessTokenResult
  { accessToken :: Text,
    refreshToken :: Text,
    issuedAt :: UTCTime,
    accessTokenExpiresAt :: UTCTime,
    refreshTokenExpiresAt :: UTCTime
  }
  deriving (Show, Generic, FromJSON, ToJSON)
