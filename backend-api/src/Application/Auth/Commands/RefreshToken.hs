{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Application.Auth.Commands.RefreshToken where

import Data.Aeson (FromJSON, ToJSON)
import Data.Text (Text)
import GHC.Generics (Generic)

data RefreshTokenRequest = RefreshTokenRequest
  { refreshToken :: Text
  }
  deriving (Show, Generic, FromJSON, ToJSON)

data RefreshTokenResponse = RefreshTokenResponse
  { accessToken :: Text,
    expiresIn :: Int
  }
  deriving (Show, Generic, FromJSON, ToJSON)
