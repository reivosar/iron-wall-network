{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Domain.DomainEventPublisher
  ( DomainEventPublisher (..),
    DomainEventError (..),
  )
where

import Data.Aeson (ToJSON, Value)
import Data.UUID (UUID)
import GHC.Generics (Generic)

data DomainEventError
  = PublishEventFailed String
  | UnexpectedError String
  deriving (Show, Generic, ToJSON)

class DomainEventPublisher m where
  publishEvent :: (ToJSON a) => UUID -> String -> String -> String -> a -> Maybe Value -> m (Either DomainEventError ())
