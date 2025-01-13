{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Domain.BankAccount.Events.AccountActivated (AccountActivated (..), eventName) where

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

eventName :: Text
eventName = "AccountActivated"
