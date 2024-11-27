{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module Application.BankAccount.Commands.SubmitAccountForApproval where

import GHC.Generics (Generic)
import Data.Aeson (FromJSON, ToJSON)
import Data.UUID (UUID)
import Data.Text (Text)
import Data.Maybe (fromMaybe)

data SubmitAccountForApproval = SubmitAccountForApproval
  { accountId :: UUID
  , reason :: Maybe Text
  } deriving (Show, Generic, FromJSON, ToJSON)
