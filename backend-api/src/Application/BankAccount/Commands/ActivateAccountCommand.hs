{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Application.BankAccount.Commands.ActivateAccountCommand where

import Data.Aeson (FromJSON, ToJSON)
import Data.UUID (UUID)
import GHC.Generics (Generic)

data ActivateAccountCommand = ActivateAccountCommand
  { accountId :: UUID
  }
  deriving (Show, Generic, FromJSON, ToJSON)
