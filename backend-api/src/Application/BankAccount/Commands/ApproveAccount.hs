{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Application.BankAccount.Commands.ApproveAccount where

import Data.Aeson (FromJSON, ToJSON)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.UUID (UUID)
import GHC.Generics (Generic)

data ApproveAccount = ApproveAccount
  { accountId :: UUID,
    approvalNotes :: Maybe Text
  }
  deriving (Show, Generic, FromJSON, ToJSON)