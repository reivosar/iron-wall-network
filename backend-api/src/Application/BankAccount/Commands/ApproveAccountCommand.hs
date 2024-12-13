{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Application.BankAccount.Commands.ApproveAccountCommand (ApproveAccountCommand (..)) where

import Data.Aeson (FromJSON, ToJSON)
import Data.Maybe ()
import Data.Text (Text)
import Data.UUID (UUID)
import GHC.Generics (Generic)

data ApproveAccountCommand = ApproveAccountCommand
  { accountId :: UUID,
    approvalNotes :: Maybe Text
  }
  deriving (Show, Generic, FromJSON, ToJSON)
