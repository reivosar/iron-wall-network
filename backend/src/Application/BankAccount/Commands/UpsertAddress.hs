{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module Application.BankAccount.Commands.UpsertAddress where

import GHC.Generics (Generic)
import Data.Aeson (FromJSON, ToJSON)
import Data.UUID (UUID)
import Data.Text (Text)

data UpsertAddress = UpsertAddress
  { accountId :: UUID 
  , address :: Text
  , addressType :: Text
  } deriving (Show, Generic, FromJSON, ToJSON)
