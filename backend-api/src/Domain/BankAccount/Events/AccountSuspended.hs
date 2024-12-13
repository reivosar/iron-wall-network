{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Domain.BankAccount.Events.AccountSuspended (AccountSuspended (..)) where

import Data.Aeson (FromJSON, ToJSON)
import Data.Maybe ()
import Data.Text (Text)
import Data.Time.Clock (UTCTime)
import Data.UUID (UUID)
import GHC.Generics (Generic)

data AccountSuspended = AccountSuspended
  { accountId :: UUID,
    reason :: Maybe Text,
    suspendedAt :: UTCTime
  }
  deriving (Show, Generic, FromJSON, ToJSON)
