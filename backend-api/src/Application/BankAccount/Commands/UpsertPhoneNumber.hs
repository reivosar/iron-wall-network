{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module Application.BankAccount.Commands.UpsertPhoneNumber where

import GHC.Generics (Generic)
import Data.Aeson (FromJSON, ToJSON)
import Data.UUID (UUID)
import Data.Text (Text)

data UpsertPhoneNumber = UpsertPhoneNumber
  { accountId :: UUID 
  , phoneNumber :: Text
  , phoneType :: Text
  } deriving (Show, Generic, FromJSON, ToJSON)
