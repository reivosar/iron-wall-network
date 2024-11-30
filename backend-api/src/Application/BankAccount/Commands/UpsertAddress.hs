{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Application.BankAccount.Commands.UpsertAddress where

import Data.Aeson (FromJSON, ToJSON)
import Data.Text (Text)
import Data.UUID (UUID)
import GHC.Generics (Generic)

data UpsertAddress = UpsertAddress
  { accountId :: UUID,
    address :: Text,
    addressType :: Text
  }
  deriving (Show, Generic, FromJSON, ToJSON)
