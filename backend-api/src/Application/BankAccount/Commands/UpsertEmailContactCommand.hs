{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Application.BankAccount.Commands.UpsertEmailContactCommand (UpsertEmailContactCommand (..)) where

import Data.Aeson (FromJSON, ToJSON)
import Data.Text (Text)
import Data.UUID (UUID)
import GHC.Generics (Generic)

data UpsertEmailContactCommand = UpsertEmailContactCommand
  { accountId :: UUID,
    email :: Text
  }
  deriving (Show, Generic, FromJSON, ToJSON)
