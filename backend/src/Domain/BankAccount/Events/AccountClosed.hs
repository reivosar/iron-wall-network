{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module Domain.BankAccount.Events.AccountClosed where

import Data.Time.Clock (UTCTime)
import Data.UUID (UUID)
import GHC.Generics (Generic)
import Data.Aeson (FromJSON, ToJSON)

data AccountClosed = AccountClosed
  { accountId :: UUID
  , closedAt :: UTCTime
  } deriving (Show, Generic, FromJSON, ToJSON)
