{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module Application.BankAccount.Commands.UpsertUserContactInfo where

import GHC.Generics (Generic)
import Data.Aeson (FromJSON, ToJSON)
import Data.UUID (UUID)
import Data.Text (Text)

data UpsertUserContactInfo = UpsertUserContactInfo
  { accountId :: UUID 
  , email :: Text 
  } deriving (Show, Generic, FromJSON, ToJSON)
