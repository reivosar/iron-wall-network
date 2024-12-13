{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Application.BankAccount.Commands.SuspendAccountCommand (SuspendAccountCommand (..)) where

import Data.Aeson (FromJSON, ToJSON)
import Data.Maybe ()
import Data.Text (Text)
import Data.UUID (UUID)
import GHC.Generics (Generic)

data SuspendAccountCommand = SuspendAccountCommand
  { accountId :: UUID,
    reason :: Maybe Text
  }
  deriving (Show, Generic, FromJSON, ToJSON)
