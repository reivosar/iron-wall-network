{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Application.BankAccount.Commands.ActivateAccountCommand where

import Data.Aeson (FromJSON, ToJSON)
import Data.Text (Text)
import Data.UUID (UUID)
import GHC.Generics (Generic)

data ActivateAccountCommand = ActivateAccountCommand
  { accountId :: UUID,
    password :: Text
  }
  deriving (Show, Generic, FromJSON, ToJSON)
