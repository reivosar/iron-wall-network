{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Application.Auth.Services.UserRefreshTokenDto where

import Data.Aeson (FromJSON, ToJSON)
import Data.Text (Text)
import Data.Time (UTCTime)
import GHC.Generics (Generic)

data UserRefreshTokenDto = UserRefreshTokenDto
  { userId :: Int,
    refreshToken :: Text,
    issuedAt :: UTCTime,
    expiresAt :: UTCTime
  }
  deriving (Show, Generic, FromJSON, ToJSON)
