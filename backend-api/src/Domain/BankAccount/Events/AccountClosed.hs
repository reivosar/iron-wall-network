{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module Domain.BankAccount.Events.AccountClosed where

import Data.Time.Clock (UTCTime)
import Data.UUID (UUID)
import GHC.Generics (Generic)
import Data.Aeson (FromJSON, ToJSON)
import Data.Text (Text)
import Data.Maybe (fromMaybe)

data AccountClosed = AccountClosed
  { accountId :: UUID
    , reason :: Maybe Text
    , closedAt :: UTCTime
  } deriving (Show, Generic, FromJSON, ToJSON)
