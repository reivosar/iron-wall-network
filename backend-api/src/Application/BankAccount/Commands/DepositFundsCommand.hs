{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Application.BankAccount.Commands.DepositFundsCommand (DepositFundsCommand (..)) where

import Data.Aeson (FromJSON, ToJSON)
import Data.UUID (UUID)
import GHC.Generics (Generic)

data DepositFundsCommand = DepositFundsCommand
  { accountId :: UUID,
    depositAmount :: Double
  }
  deriving (Show, Generic, FromJSON, ToJSON)
