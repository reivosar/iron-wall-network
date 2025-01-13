{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}

module Domain.Event
  ( Event (..),
    EventDataParser (..),
  )
where

import Data.Aeson (FromJSON, Value, decode, encode)
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

class EventDataParser a where
  parseEventData :: Event -> Maybe a

instance (FromJSON a) => EventDataParser a where
  parseEventData event =
    decode (encode (eventData event))
