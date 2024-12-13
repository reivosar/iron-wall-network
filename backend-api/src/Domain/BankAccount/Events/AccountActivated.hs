{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Domain.BankAccount.Events.AccountActivated (AccountActivated (..)) where

import Data.Aeson (FromJSON, ToJSON)
import Data.Text (Text)
import Data.Time.Clock (UTCTime)
import Data.UUID (UUID)
import GHC.Generics (Generic)

data AccountActivated = AccountActivated
  { accountId :: UUID,
    password :: Text,
    activatedAt :: UTCTime
  }
  deriving (Show, Generic, FromJSON, ToJSON)
