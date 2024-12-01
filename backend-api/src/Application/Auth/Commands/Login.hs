{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Application.Auth.Commands.Login where

import Data.Aeson (FromJSON, ToJSON)
import Data.Text (Text)
import GHC.Generics (Generic)

data LoginRequest = LoginRequest
  { username :: Text,
    password :: Text,
    authKey :: Text
  }
  deriving (Show, Generic, FromJSON, ToJSON)

data TokenResponse = TokenResponse
  { accessToken :: Text,
    refreshToken :: Text,
    expiresIn :: Int
  }
  deriving (Show, Generic, FromJSON, ToJSON)
