{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module Application.BankAccount.Commands.DepositFunds where

import GHC.Generics (Generic)
import Data.Aeson (FromJSON, ToJSON)
import Data.UUID (UUID)

data DepositFunds = DepositFunds
  { accountId :: UUID
  , depositAmount :: Double
  } deriving (Show, Generic, FromJSON, ToJSON)
