{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Application.BankAccount.Commands.WithdrawFundsCommand where

import Data.Aeson (FromJSON, ToJSON)
import Data.UUID (UUID)
import GHC.Generics (Generic)

data WithdrawFundsCommand = WithdrawFundsCommand
  { accountId :: UUID,
    withdrawAmount :: Double
  }
  deriving (Show, Generic, FromJSON, ToJSON)
