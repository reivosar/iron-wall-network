{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Application.Auth.Services.RecreateAccessTokenResult (RecreateAccessTokenResult (..)) where

import Data.Aeson (FromJSON, ToJSON)
import Data.Text (Text)
import Data.Time (UTCTime)
import GHC.Generics (Generic)

data RecreateAccessTokenResult = RecreateAccessTokenResult
  { accessToken :: Text,
    issuedAt :: UTCTime,
    accessTokenExpiresAt :: UTCTime
  }
  deriving (Show, Generic, FromJSON, ToJSON)
