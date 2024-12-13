{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Application.Auth.Services.UserAccessTokenDto (UserAccessTokenDto (..)) where

import Data.Aeson (FromJSON, ToJSON)
import Data.Text (Text)
import Data.Time (UTCTime)
import GHC.Generics (Generic)

data UserAccessTokenDto = UserAccessTokenDto
  { userId :: Int,
    accessToken :: Text,
    issuedAt :: UTCTime,
    expiresAt :: UTCTime
  }
  deriving (Show, Generic, FromJSON, ToJSON)
