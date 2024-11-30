{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Application.BankAccount.Commands.DepositFunds where

import Data.Aeson (FromJSON, ToJSON)
import Data.UUID (UUID)
import GHC.Generics (Generic)

data DepositFunds = DepositFunds
  { accountId :: UUID,
    depositAmount :: Double
  }
  deriving (Show, Generic, FromJSON, ToJSON)
