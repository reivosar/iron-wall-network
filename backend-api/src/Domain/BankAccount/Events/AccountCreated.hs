{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module Domain.BankAccount.Events.AccountCreated where

import Data.Text (Text)
import Data.Time.Clock (UTCTime)
import Data.UUID (UUID)
import GHC.Generics (Generic)
import Data.Aeson (FromJSON, ToJSON)

data AccountCreated = AccountCreated
  { accountId :: UUID
  , username :: Text
  , fullName :: Text
  , email :: Text
  , createdAt :: UTCTime
  } deriving (Show, Generic, FromJSON, ToJSON)
