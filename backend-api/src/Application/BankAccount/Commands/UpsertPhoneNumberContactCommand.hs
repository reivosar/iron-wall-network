{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Application.BankAccount.Commands.UpsertPhoneNumberContactCommand (UpsertPhoneNumberContactCommand (..)) where

import Data.Aeson (FromJSON, ToJSON)
import Data.Text (Text)
import Data.UUID (UUID)
import GHC.Generics (Generic)

data UpsertPhoneNumberContactCommand = UpsertPhoneNumberContactCommand
  { accountId :: UUID,
    phoneNumber :: Text,
    phoneType :: Text
  }
  deriving (Show, Generic, FromJSON, ToJSON)
