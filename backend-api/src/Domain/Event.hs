{-# LANGUAGE DeriveGeneric #-}

module Domain.Event
  ( Event (..),
  )
where

import Data.Aeson (Value)
import Data.Text (Text)
import Data.Time.Clock (UTCTime)
import GHC.Generics (Generic)

data Event = Event
  { aggregateId :: Text,
    aggregateType :: Text,
    eventType :: Text,
    eventData :: Value,
    sequenceNumber :: Integer,
    version :: Integer,
    triggeredBy :: Maybe Text,
    occurredAt :: UTCTime,
    metadata :: Maybe Value
  }
  deriving (Show, Generic)
