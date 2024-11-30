{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Application.BankAccount.Commands.ActivateAccount where

import Data.Aeson (FromJSON, ToJSON)
import Data.UUID (UUID)
import GHC.Generics (Generic)

data ActivateAccount = ActivateAccount
  { accountId :: UUID
  }
  deriving (Show, Generic, FromJSON, ToJSON)
