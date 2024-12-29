{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Domain.DomainEventSerializer
  ( DomainEventSerializer (..),
    SerializerError (..),
  )
where

import Data.Aeson (FromJSON, ToJSON, eitherDecode, encode, object, withObject, (.:), (.:?), (.=))
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy as BSL
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time.Clock (UTCTime)
import Domain.DomainEventStore (Event (..))
import GHC.Generics (Generic)

data SerializerError
  = SerializationError T.Text
  | DeserializationError T.Text
  deriving (Show, Eq)

class DomainEventSerializer m where
  serialize :: Event -> m (Either SerializerError BSL.ByteString)
  deserialize :: BSL.ByteString -> m (Either SerializerError Event)
