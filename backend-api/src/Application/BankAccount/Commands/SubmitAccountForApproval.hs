{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Application.BankAccount.Commands.SubmitAccountForApproval where

import Data.Aeson (FromJSON, ToJSON)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.UUID (UUID)
import GHC.Generics (Generic)

data SubmitAccountForApproval = SubmitAccountForApproval
  { accountId :: UUID,
    reason :: Maybe Text
  }
  deriving (Show, Generic, FromJSON, ToJSON)
