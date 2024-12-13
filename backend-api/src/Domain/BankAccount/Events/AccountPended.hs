{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Domain.BankAccount.Events.AccountPended (AccountPended (..)) where

import Data.Aeson (FromJSON, ToJSON)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Time.Clock (UTCTime)
import Data.UUID (UUID)
import GHC.Generics (Generic)

data AccountPended = AccountPended
  { accountId :: UUID,
    reason :: Maybe Text,
    pendedAt :: UTCTime
  }
  deriving (Show, Generic, FromJSON, ToJSON)
