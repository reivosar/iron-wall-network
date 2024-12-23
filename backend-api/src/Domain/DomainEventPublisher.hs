{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Domain.DomainEventPublisher
  ( DomainEventPublisher (..),
    DomainEventError (..),
  )
where

import Data.Aeson (ToJSON, Value)
import Data.Text (Text)
import Data.UUID (UUID)
import GHC.Generics (Generic)

data DomainEventError
  = PublishEventFailed Text
  | UnexpectedError Text
  deriving (Show, Generic, ToJSON)

class DomainEventPublisher m where
  publishEvent :: (ToJSON a) => UUID -> Text -> Text -> Text -> a -> Maybe Value -> m (Either DomainEventError ())
