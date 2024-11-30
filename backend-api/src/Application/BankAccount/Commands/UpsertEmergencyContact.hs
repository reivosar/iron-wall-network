{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Application.BankAccount.Commands.UpsertEmergencyContact where

import Data.Aeson (FromJSON, ToJSON)
import Data.Text (Text)
import Data.UUID (UUID)
import GHC.Generics (Generic)

data UpsertEmergencyContact = UpsertEmergencyContact
  { accountId :: UUID,
    contactName :: Text,
    contactPhone :: Text
  }
  deriving (Show, Generic, FromJSON, ToJSON)
