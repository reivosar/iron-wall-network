{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Domain.BankAccount.Events.UserContactInfoUpserted (UserContactInfoUpserted (..)) where

import Data.Aeson (FromJSON, ToJSON)
import Data.Text (Text)
import Data.Time.Clock (UTCTime)
import Data.UUID (UUID)
import GHC.Generics (Generic)

data UserContactInfoUpserted = UserContactInfoUpserted
  { accountId :: UUID,
    email :: Text,
    updatedAt :: UTCTime
  }
  deriving (Show, Generic, FromJSON, ToJSON)
