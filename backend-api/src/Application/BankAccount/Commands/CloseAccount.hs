{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Application.BankAccount.Commands.CloseAccount where

import Data.Aeson (FromJSON, ToJSON)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.UUID (UUID)
import GHC.Generics (Generic)

data CloseAccount = CloseAccount
  { accountId :: UUID,
    reason :: Maybe Text
  }
  deriving (Show, Generic, FromJSON, ToJSON)
