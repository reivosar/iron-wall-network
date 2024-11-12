{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module Domain.BankAccount.Events.UserContactInfoUpserted where

import Data.Text (Text)
import Data.Time.Clock (UTCTime)
import Data.UUID (UUID)
import GHC.Generics (Generic)
import Data.Aeson (FromJSON, ToJSON)

data UserContactInfoUpserted = UserContactInfoUpserted
  { accountId :: UUID
  , email :: Text
  , updatedAt :: UTCTime
  } deriving (Show, Generic, FromJSON, ToJSON)
