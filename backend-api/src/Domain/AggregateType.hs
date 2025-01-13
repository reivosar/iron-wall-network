{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Domain.AggregateType (AggregateType (..), aggregateTypeToText) where

import Data.Aeson (FromJSON, ToJSON)
import Data.Text (Text)
import GHC.Generics (Generic)

data AggregateType
  = Account
  deriving (Show, Eq, Enum, Bounded, Generic, FromJSON, ToJSON)

aggregateTypeToText :: AggregateType -> Text
aggregateTypeToText Account = "account"
