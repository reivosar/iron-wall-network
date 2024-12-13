{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Domain.BankAccount.Events.AddressUpserted where

import Data.Aeson (FromJSON, ToJSON)
import Data.Text (Text)
import Data.Time.Clock (UTCTime)
import Data.UUID (UUID)
import GHC.Generics (Generic)

data AddressUpserted = AddressUpserted
  { accountId :: UUID,
    address :: Text,
    addressType :: Text,
    updatedAt :: UTCTime
  }
  deriving (Show, Generic, FromJSON, ToJSON)
