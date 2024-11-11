{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module Application.BankAccount.Commands.ActivateAccount where

import GHC.Generics (Generic)
import Data.Aeson (FromJSON, ToJSON)
import Data.UUID (UUID)

data ActivateAccount = ActivateAccount
  { accountId :: UUID
  } deriving (Show, Generic, FromJSON, ToJSON)
