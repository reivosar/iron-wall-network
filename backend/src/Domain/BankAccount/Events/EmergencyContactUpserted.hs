{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module Domain.BankAccount.Events.EmergencyContactUpserted where

import Data.Text (Text)
import Data.Time.Clock (UTCTime)
import Data.UUID (UUID)
import GHC.Generics (Generic)
import Data.Aeson (FromJSON, ToJSON)

data EmergencyContactUpserted = EmergencyContactUpserted
  { accountId :: UUID
  , contactName :: Text
  , contactPhone :: Text
  , updatedAt :: UTCTime
  } deriving (Show, Generic, FromJSON, ToJSON)
