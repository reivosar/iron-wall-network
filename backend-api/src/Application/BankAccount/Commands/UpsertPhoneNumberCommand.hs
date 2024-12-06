{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Application.BankAccount.Commands.UpsertPhoneNumberCommand where

import Data.Aeson (FromJSON, ToJSON)
import Data.Text (Text)
import Data.UUID (UUID)
import GHC.Generics (Generic)

data UpsertPhoneNumberCommand = UpsertPhoneNumberCommand
  { accountId :: UUID,
    phoneNumber :: Text,
    phoneType :: Text
  }
  deriving (Show, Generic, FromJSON, ToJSON)
