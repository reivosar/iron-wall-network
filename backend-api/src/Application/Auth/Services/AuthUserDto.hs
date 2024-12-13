{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Application.Auth.Services.AuthUserDto (AuthUserDto (..)) where

import Data.Aeson (FromJSON, ToJSON)
import Data.Text (Text)
import GHC.Generics (Generic)

data AuthUserDto = AuthUserDto
  { userId :: Int,
    username :: Text,
    passwordHash :: Text,
    authKeyHash :: Text
  }
  deriving (Show, Generic, FromJSON, ToJSON)
