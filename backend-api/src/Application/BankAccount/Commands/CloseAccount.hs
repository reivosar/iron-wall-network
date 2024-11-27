{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module Application.BankAccount.Commands.CloseAccount where

import GHC.Generics (Generic)
import Data.Aeson (FromJSON, ToJSON)
import Data.UUID (UUID)
import Data.Text (Text)
import Data.Maybe (fromMaybe)

data CloseAccount = CloseAccount
  { accountId :: UUID
    , reason :: Maybe Text
  } deriving (Show, Generic, FromJSON, ToJSON)
