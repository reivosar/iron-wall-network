{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Domain.BankAccount.Events.FundsWithdrawn where

import Data.Aeson (FromJSON, ToJSON)
import Data.Time.Clock (UTCTime)
import Data.UUID (UUID)
import GHC.Generics (Generic)

data FundsWithdrawn = FundsWithdrawn
  { accountId :: UUID,
    amount :: Double,
    withdrawnAt :: UTCTime
  }
  deriving (Show, Generic, FromJSON, ToJSON)
