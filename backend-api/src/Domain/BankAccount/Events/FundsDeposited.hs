{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Domain.BankAccount.Events.FundsDeposited (FundsDeposited (..)) where

import Data.Aeson (FromJSON, ToJSON)
import Data.Time (UTCTime)
import Data.UUID (UUID)
import GHC.Generics (Generic)

data FundsDeposited = FundsDeposited
  { accountId :: UUID,
    amount :: Double,
    totalBalance :: Double,
    depositedAt :: UTCTime
  }
  deriving (Show, Generic, FromJSON, ToJSON)
