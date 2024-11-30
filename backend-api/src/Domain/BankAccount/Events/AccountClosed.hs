{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Domain.BankAccount.Events.AccountClosed where

import Data.Aeson (FromJSON, ToJSON)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Time.Clock (UTCTime)
import Data.UUID (UUID)
import GHC.Generics (Generic)

data AccountClosed = AccountClosed
  { accountId :: UUID,
    reason :: Maybe Text,
    closedAt :: UTCTime
  }
  deriving (Show, Generic, FromJSON, ToJSON)
