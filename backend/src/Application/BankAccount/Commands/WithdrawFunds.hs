{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module Application.BankAccount.Commands.WithdrawFunds where

import GHC.Generics (Generic)
import Data.Aeson (FromJSON, ToJSON)
import Data.UUID (UUID)

data WithdrawFunds = WithdrawFunds
  { accountId :: UUID 
  , withdrawAmount :: Double
  } deriving (Show, Generic, FromJSON, ToJSON)
