{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module Domain.BankAccount.Events.AccountPended where

import Data.Text (Text)
import Data.Time.Clock (UTCTime)
import Data.UUID (UUID)
import GHC.Generics (Generic)
import Data.Aeson (FromJSON, ToJSON)

data AccountPended = AccountPended
  { accountId :: UUID
  , accountHolderName :: Text
  , reason :: Text
  , pendedAt :: UTCTime
  } deriving (Show, Generic, FromJSON, ToJSON)
