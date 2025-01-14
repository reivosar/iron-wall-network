{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UndecidableInstances #-}

module Infrastructure.Repositories.BankAccount.EventStoreEmergencyContactRepository (findById, save) where

import qualified Data.UUID as UUID
import Domain.AggregateType (AggregateType (..), aggregateTypeToText)
import Domain.BankAccount.Entity.EmergencyContact (parseEmergencyContactFromEvent)
import qualified Domain.BankAccount.Events.EmergencyContactUpserted as EmergencyContactUpserted
import Domain.BankAccount.Repositories.EmergencyContactRepository
import Domain.BankAccount.ValueObject.AccountId (unwrapAccountId)
import Domain.DomainEventStore
import Domain.Event (parseEventData)
import GHC.Exception (toException)

instance (DomainEventStore m) => EmergencyContactRepository m where
  findById accountId = do
    let aggregateType = (aggregateTypeToText Account)
        eventNames = [EmergencyContactUpserted.eventName]
        aggregateId = UUID.toText (unwrapAccountId accountId)

    eventsResult <- getLatestEventsByAggregateAndEventNames aggregateId aggregateType eventNames

    case eventsResult of
      Left err -> return $ Left err
      Right [] -> return $ Right Nothing
      Right (event : _) ->
        case parseEventData event >>= parseEmergencyContactFromEvent of
          Nothing ->
            return $ Left (toException (userError "Failed to parse entity from event data"))
          Just validEntity ->
            return $ Right (Just validEntity)

  save _ = do
    return $ Right ()
