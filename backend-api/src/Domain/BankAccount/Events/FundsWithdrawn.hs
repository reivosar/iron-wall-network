{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Domain.BankAccount.Events.FundsWithdrawn (FundsWithdrawn (..), eventName) where

import Data.Aeson (FromJSON, ToJSON)
import Data.Text (Text)
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

eventName :: Text
eventName = "FundsWithdrawn"
