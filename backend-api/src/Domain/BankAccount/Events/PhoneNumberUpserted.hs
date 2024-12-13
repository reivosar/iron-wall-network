{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Domain.BankAccount.Events.PhoneNumberUpserted (PhoneNumberUpserted (..)) where

import Data.Aeson (FromJSON, ToJSON)
import Data.Text (Text)
import Data.Time.Clock (UTCTime)
import Data.UUID (UUID)
import GHC.Generics (Generic)

data PhoneNumberUpserted = PhoneNumberUpserted
  { accountId :: UUID,
    phoneNumber :: Text,
    phoneType :: Text,
    updatedAt :: UTCTime
  }
  deriving (Show, Generic, FromJSON, ToJSON)
