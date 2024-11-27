{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module Domain.BankAccount.Events.AccountPended where

import Data.Text (Text)
import Data.Time.Clock (UTCTime)
import Data.UUID (UUID)
import GHC.Generics (Generic)
import Data.Aeson (FromJSON, ToJSON)
import Data.Maybe (fromMaybe)

data AccountPended = AccountPended
  { accountId :: UUID
  , reason :: Maybe Text
  , pendedAt :: UTCTime
  } deriving (Show, Generic, FromJSON, ToJSON)
