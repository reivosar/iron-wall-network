{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}

module Infrastructure.Services.Shared.EventStoreEventStatusValidator
  ( validateEventStatus,
  )
where

import Control.Exception (SomeException)
import Data.List (find)
import Data.Text (Text, pack)
import qualified Data.Text as T
import Domain.AggregateId (unwrapAggregateId)
import Domain.AggregateType (aggregateTypeToText)
import Domain.DomainEventStore (DomainEventStore, getLatestEventsByAggregateAndEventNames)
import Domain.Error (DomainError, mkDomainError)
import Domain.Event (Event)
import qualified Domain.Event as DE
import Domain.Shared.Services.EventStatusValidator (EventStatusValidator (..))

instance (DomainEventStore m) => EventStatusValidator m where
  validateEventStatus aggregateId aggregateType requiredEvents prohibitedEvents = do
    let aggregateIdText = unwrapAggregateId aggregateId
        aggregateTypeText = aggregateTypeToText aggregateType

    eventsResult <- fetchEvents aggregateIdText aggregateTypeText (requiredEvents ++ prohibitedEvents)
    case eventsResult of
      Left err -> return $ Left $ mkDomainError $ T.pack $ show err
      Right events -> return $ validateEventConditions requiredEvents prohibitedEvents events

fetchEvents ::
  (DomainEventStore m) =>
  Text -> -- Aggregate ID
  Text -> -- Aggregate Type
  [Text] -> -- Event Names
  m (Either SomeException [Event])
fetchEvents aggregateId aggregateType eventNames =
  getLatestEventsByAggregateAndEventNames aggregateId aggregateType eventNames

validateEventConditions ::
  [Text] -> -- Required Events
  [Text] -> -- Prohibited Events
  [Event] ->
  Either DomainError ()
validateEventConditions requiredEvents prohibitedEvents events =
  case (missingRequiredEvent, existingProhibitedEvent) of
    (Just missing, _) -> Left $ mkDomainError $ pack ("Missing required event: " <> show missing)
    (_, Just existing) -> Left $ mkDomainError $ pack ("Prohibited event exists: " <> show existing)
    (Nothing, Nothing) -> Right ()
  where
    missingRequiredEvent = find (\eventName -> not (hasEvent eventName events)) requiredEvents
    existingProhibitedEvent = find (\eventName -> hasEvent eventName events) prohibitedEvents

hasEvent :: Text -> [DE.Event] -> Bool
hasEvent eventNameToFind = any (\event -> DE.eventType event == eventNameToFind)
