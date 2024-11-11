{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module Application.BankAccount.Commands.ApproveAccount where

import GHC.Generics (Generic)
import Data.Aeson (FromJSON, ToJSON)
import Data.UUID (UUID)
import Data.Text (Text)

data ApproveAccount = ApproveAccount
  { accountId :: UUID
  , approvalNotes :: Text
  } deriving (Show, Generic, FromJSON, ToJSON)
