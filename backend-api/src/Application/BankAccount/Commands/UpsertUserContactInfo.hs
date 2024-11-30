{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Application.BankAccount.Commands.UpsertUserContactInfo where

import Data.Aeson (FromJSON, ToJSON)
import Data.Text (Text)
import Data.UUID (UUID)
import GHC.Generics (Generic)

data UpsertUserContactInfo = UpsertUserContactInfo
  { accountId :: UUID,
    email :: Text
  }
  deriving (Show, Generic, FromJSON, ToJSON)