{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Domain.BankAccount.Events.AccountApproved (AccountApproved (..)) where

import Data.Aeson (FromJSON, ToJSON)
import Data.Maybe ()
import Data.Text (Text)
import Data.Time.Clock (UTCTime)
import Data.UUID (UUID)
import GHC.Generics (Generic)

data AccountApproved = AccountApproved
  { accountId :: UUID,
    approvedAt :: UTCTime,
    approvalNotes :: Maybe Text
  }
  deriving (Show, Generic, FromJSON, ToJSON)
