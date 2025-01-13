{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Domain.BankAccount.Events.AddressUpserted (AddressUpserted (..), eventName) where

import Data.Aeson (FromJSON, ToJSON)
import Data.Text (Text)
import Data.Time.Clock (UTCTime)
import Data.UUID (UUID)
import GHC.Generics (Generic)

data AddressUpserted = AddressUpserted
  { accountId :: UUID,
    postalCode :: Text,
    prefecture :: Text,
    city :: Text,
    townArea :: Text,
    buildingName :: Maybe Text,
    addressType :: Text,
    updatedAt :: UTCTime
  }
  deriving (Show, Generic, FromJSON, ToJSON)

eventName :: Text
eventName = "AddressUpserted"
