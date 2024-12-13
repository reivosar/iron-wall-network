{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Domain.BankAccount.Events.AccountCreated (AccountCreated (..)) where

import Data.Aeson (FromJSON, ToJSON)
import Data.Text (Text)
import Data.Time.Clock (UTCTime)
import Data.UUID (UUID)
import GHC.Generics (Generic)

data AccountCreated = AccountCreated
  { accountId :: UUID,
    username :: Text,
    fullName :: Text,
    email :: Text,
    createdAt :: UTCTime
  }
  deriving (Show, Generic, FromJSON, ToJSON)
