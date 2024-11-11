{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module Application.BankAccount.Commands.SubmitAccountForApproval where

import GHC.Generics (Generic)
import Data.Aeson (FromJSON, ToJSON)
import Data.UUID (UUID)
import Data.Text (Text)

data SubmitAccountForApproval = SubmitAccountForApproval
  { accountId :: UUID
  , submitterNotes :: Text
  } deriving (Show, Generic, FromJSON, ToJSON)
