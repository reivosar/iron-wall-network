{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Application.BankAccount.Commands.WithdrawFunds where

import Data.Aeson (FromJSON, ToJSON)
import Data.UUID (UUID)
import GHC.Generics (Generic)

data WithdrawFunds = WithdrawFunds
  { accountId :: UUID,
    withdrawAmount :: Double
  }
  deriving (Show, Generic, FromJSON, ToJSON)
