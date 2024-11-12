{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module Domain.BankAccount.Events.FundsWithdrawn where

import Data.Time.Clock (UTCTime)
import Data.UUID (UUID)
import GHC.Generics (Generic)
import Data.Aeson (FromJSON, ToJSON)

data FundsWithdrawn = FundsWithdrawn
  { accountId :: UUID
  , amount :: Double
  , withdrawnAt :: UTCTime
  } deriving (Show, Generic, FromJSON, ToJSON)
