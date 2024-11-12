{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module Domain.BankAccount.Events.FundsDeposited where

import Data.Time.Clock (UTCTime)
import Data.UUID (UUID)
import GHC.Generics (Generic)
import Data.Aeson (FromJSON, ToJSON)

data FundsDeposited = FundsDeposited
  { accountId :: UUID
  , amount :: Double
  , depositedAt :: UTCTime
  } deriving (Show, Generic, FromJSON, ToJSON)
