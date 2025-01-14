{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UndecidableInstances #-}

module Infrastructure.Repositories.BankAccount.EventStorePhoneNumberRepository (findById, save) where

import qualified Data.UUID as UUID
import Domain.AggregateType (AggregateType (..), aggregateTypeToText)
import Domain.BankAccount.Entity.PhoneNumberContact (parsePhoneNumberContactFromEvent)
import qualified Domain.BankAccount.Events.PhoneNumberContactUpserted as PhoneNumberContactUpserted
import Domain.BankAccount.Repositories.PhoneNumberRepository
import Domain.BankAccount.ValueObject.AccountId (unwrapAccountId)
import Domain.DomainEventStore
import Domain.Event (parseEventData)
import GHC.Exception (toException)

instance (DomainEventStore m) => PhoneNumberRepository m where
  findById accountId = do
    let aggregateType = aggregateTypeToText Account
        eventNames = [PhoneNumberContactUpserted.eventName]
        aggregateId = UUID.toText (unwrapAccountId accountId)

    eventsResult <- getLatestEventsByAggregateAndEventNames aggregateId aggregateType eventNames

    case eventsResult of
      Left err -> return $ Left err
      Right [] -> return $ Right Nothing
      Right (event : _) ->
        case parseEventData event >>= parsePhoneNumberContactFromEvent of
          Nothing ->
            return $ Left (toException (userError "Failed to parse entity from event data"))
          Just validEntity ->
            return $ Right (Just validEntity)

  save _ = do
    return $ Right ()
