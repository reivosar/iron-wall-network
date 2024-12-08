{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Domain.BankAccount.Events.FundsWithdrawn (FundsWithdrawn (..)) where

import Data.Aeson (FromJSON, ToJSON)
import Data.Time (UTCTime)
import Data.UUID (UUID)
import GHC.Generics (Generic)

data FundsWithdrawn = FundsWithdrawn
  { accountId :: UUID,
    amount :: Double,
    totalBalance :: Double,
    withdrawnAt :: UTCTime
  }
  deriving (Show, Generic, FromJSON, ToJSON)
