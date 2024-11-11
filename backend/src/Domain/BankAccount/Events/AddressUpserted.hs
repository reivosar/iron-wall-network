{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module Domain.BankAccount.Events.AddressUpserted where

import Data.Text (Text)
import Data.Time.Clock (UTCTime)
import Data.UUID (UUID)
import GHC.Generics (Generic)
import Data.Aeson (FromJSON, ToJSON)

data AddressUpserted = AddressUpserted
  { accountId :: UUID
  , address :: Text
  , addressType :: Text
  , updatedAt :: UTCTime
  } deriving (Show, Generic, FromJSON, ToJSON)
