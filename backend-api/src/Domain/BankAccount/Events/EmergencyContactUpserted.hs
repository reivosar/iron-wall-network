{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Domain.BankAccount.Events.EmergencyContactUpserted (EmergencyContactUpserted (..)) where

import Data.Aeson (FromJSON, ToJSON)
import Data.Text (Text)
import Data.Time.Clock (UTCTime)
import Data.UUID (UUID)
import GHC.Generics (Generic)

data EmergencyContactUpserted = EmergencyContactUpserted
  { accountId :: UUID,
    contactName :: Text,
    contactPhone :: Text,
    updatedAt :: UTCTime
  }
  deriving (Show, Generic, FromJSON, ToJSON)
