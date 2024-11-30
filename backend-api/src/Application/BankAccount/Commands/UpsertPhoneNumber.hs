{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Application.BankAccount.Commands.UpsertPhoneNumber where

import Data.Aeson (FromJSON, ToJSON)
import Data.Text (Text)
import Data.UUID (UUID)
import GHC.Generics (Generic)

data UpsertPhoneNumber = UpsertPhoneNumber
  { accountId :: UUID,
    phoneNumber :: Text,
    phoneType :: Text
  }
  deriving (Show, Generic, FromJSON, ToJSON)