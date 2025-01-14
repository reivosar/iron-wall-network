{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Domain.Shared.Services.EventStatusValidator
  ( EventStatusValidator (..),
    AggregateId,
    EventName,
    RequiredEvents,
    ProhibitedEvents,
  )
where

import Data.Text (Text)
import Domain.AggregateId (AggregateId)
import Domain.AggregateType (AggregateType)
import Domain.Error (DomainError)

type EventName = Text

type RequiredEvents = [EventName]

type ProhibitedEvents = [EventName]

class (Monad m) => EventStatusValidator m where
  validateEventStatus ::
    AggregateId ->
    AggregateType ->
    RequiredEvents ->
    ProhibitedEvents ->
    m (Either DomainError ())
