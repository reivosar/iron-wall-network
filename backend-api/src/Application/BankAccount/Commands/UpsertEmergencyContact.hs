{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module Application.BankAccount.Commands.UpsertEmergencyContact where

import GHC.Generics (Generic)
import Data.Aeson (FromJSON, ToJSON)
import Data.UUID (UUID)
import Data.Text (Text)

data UpsertEmergencyContact = UpsertEmergencyContact
  { accountId :: UUID 
  , contactName :: Text
  , contactPhone :: Text
  } deriving (Show, Generic, FromJSON, ToJSON)
