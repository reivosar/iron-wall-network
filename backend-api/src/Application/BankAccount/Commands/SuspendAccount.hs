{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module Application.BankAccount.Commands.SuspendAccount where

import GHC.Generics (Generic)
import Data.Aeson (FromJSON, ToJSON)
import Data.UUID (UUID)
import Data.Text (Text)
import Data.Maybe (fromMaybe)

data SuspendAccount = SuspendAccount
  { accountId :: UUID
    , reason :: Maybe Text
  } deriving (Show, Generic, FromJSON, ToJSON)
