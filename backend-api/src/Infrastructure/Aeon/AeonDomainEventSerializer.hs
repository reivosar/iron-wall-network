{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Infrastructure.Aeon.AeonDomainEventSerializer
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
import Domain.DomainEventSerializer (DomainEventSerializer (..), SerializerError (..))
import Domain.Event (Event (..))
import GHC.Generics (Generic)

instance ToJSON Event where
  toJSON (Event aggregateId aggregateType eventType eventData sequenceNumber version triggeredBy occurredAt metadata) =
    object
      [ "aggregateId" .= aggregateId,
        "aggregateType" .= aggregateType,
        "eventType" .= eventType,
        "eventData" .= eventData,
        "sequenceNumber" .= sequenceNumber,
        "version" .= version,
        "triggeredBy" .= triggeredBy,
        "occurredAt" .= occurredAt,
        "metadata" .= metadata
      ]

instance FromJSON Event where
  parseJSON = withObject "Event" $ \v ->
    Event
      <$> v .: "aggregateId"
      <*> v .: "aggregateType"
      <*> v .: "eventType"
      <*> v .: "eventData"
      <*> v .: "sequenceNumber"
      <*> v .: "version"
      <*> v .:? "triggeredBy"
      <*> v .: "occurredAt"
      <*> v .:? "metadata"

instance DomainEventSerializer IO where
  serialize event = return $ Right $ encode event

  deserialize byteString =
    case eitherDecode byteString of
      Right event -> return $ Right event
      Left err -> return $ Left $ DeserializationError (T.pack err)
