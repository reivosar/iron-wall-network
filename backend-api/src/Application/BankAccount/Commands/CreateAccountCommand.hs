{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Application.BankAccount.Commands.CreateAccountCommand (CreateAccountCommand (..)) where

import Data.Aeson (FromJSON, ToJSON)
import Data.Text (Text)
import GHC.Generics (Generic)

data CreateAccountCommand = CreateAccountCommand
  { username :: Text,
    fullName :: Text,
    email :: Text
  }
  deriving (Show, Generic, FromJSON, ToJSON)
