{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module Application.BankAccount.Commands.CloseAccount where

import GHC.Generics (Generic)
import Data.Aeson (FromJSON, ToJSON)
import Data.UUID (UUID)

data CloseAccount = CloseAccount
  { accountId :: UUID
  } deriving (Show, Generic, FromJSON, ToJSON)
