module Domain.DomainEventPublisher
    ( DomainEventPublisher(..)
    , publishEvent
    ) where

import Data.Aeson (ToJSON)
import Data.UUID (UUID)

class DomainEventPublisher m where
    publishEvent :: ToJSON a => UUID -> String -> String -> String -> a -> m ()
