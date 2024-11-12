{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module Domain.BankAccount.Events.AccountActivated where

import Data.Time.Clock (UTCTime)
import Data.UUID (UUID)
import GHC.Generics (Generic)
import Data.Aeson (FromJSON, ToJSON)

data AccountActivated = AccountActivated
  { accountId :: UUID
  , activatedAt :: UTCTime
  } deriving (Show, Generic, FromJSON, ToJSON)
