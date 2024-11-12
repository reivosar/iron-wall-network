{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module Application.BankAccount.Commands.SuspendAccount where

import GHC.Generics (Generic)
import Data.Aeson (FromJSON, ToJSON)
import Data.UUID (UUID)
import Data.Text (Text)

data SuspendAccount = SuspendAccount
  { accountId :: UUID
  , suspendReason :: Text
  } deriving (Show, Generic, FromJSON, ToJSON)
