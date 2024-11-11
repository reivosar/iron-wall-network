{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module Domain.BankAccount.Events.PhoneNumberUpserted where

import Data.Text (Text)
import Data.Time.Clock (UTCTime)
import Data.UUID (UUID)
import GHC.Generics (Generic)
import Data.Aeson (FromJSON, ToJSON)

data PhoneNumberUpserted = PhoneNumberUpserted
  { accountId :: UUID
  , phoneNumber :: Text
  , phoneType :: Text
  , updatedAt :: UTCTime
  } deriving (Show, Generic, FromJSON, ToJSON)
