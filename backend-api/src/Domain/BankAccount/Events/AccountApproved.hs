{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module Domain.BankAccount.Events.AccountApproved where

import Data.Text (Text)
import Data.Time.Clock (UTCTime)
import Data.UUID (UUID)
import GHC.Generics (Generic)
import Data.Aeson (FromJSON, ToJSON)

data AccountApproved = AccountApproved
  { accountId :: UUID
  , approvedAt :: UTCTime
  , approvalNotes :: Text
  } deriving (Show, Generic, FromJSON, ToJSON)
