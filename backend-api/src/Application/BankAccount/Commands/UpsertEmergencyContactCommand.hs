{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Application.BankAccount.Commands.UpsertEmergencyContactCommand where

import Data.Aeson (FromJSON, ToJSON)
import Data.Text (Text)
import Data.UUID (UUID)
import GHC.Generics (Generic)

data UpsertEmergencyContactCommand = UpsertEmergencyContactCommand
  { accountId :: UUID,
    contactName :: Text,
    contactPhone :: Text
  }
  deriving (Show, Generic, FromJSON, ToJSON)
