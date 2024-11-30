{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Domain.BankAccount.Events.FundsDeposited where

import Data.Aeson (FromJSON, ToJSON)
import Data.Time.Clock (UTCTime)
import Data.UUID (UUID)
import GHC.Generics (Generic)

data FundsDeposited = FundsDeposited
  { accountId :: UUID,
    amount :: Double,
    depositedAt :: UTCTime
  }
  deriving (Show, Generic, FromJSON, ToJSON)
