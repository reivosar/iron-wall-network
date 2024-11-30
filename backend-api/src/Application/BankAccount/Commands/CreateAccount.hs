{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Application.BankAccount.Commands.CreateAccount where

import Data.Aeson (FromJSON, ToJSON)
import Data.Text (Text)
import GHC.Generics (Generic)

data CreateAccount = CreateAccount
  { username :: Text,
    fullName :: Text,
    email :: Text
  }
  deriving (Show, Generic, FromJSON, ToJSON)
