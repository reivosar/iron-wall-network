{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UndecidableInstances #-}

module Infrastructure.Repositories.EventStoreAddressRepository (findById, save) where

import Control.Monad.IO.Class (MonadIO)
import qualified Data.UUID as UUID
import Domain.AggregateType (AggregateType (..), aggregateTypeToText)
import Domain.BankAccount.Entity.Address (parseAddressFromEvent)
import qualified Domain.BankAccount.Events.AddressUpserted as AddressUpserted
import Domain.BankAccount.Repositories.AddressRepository
import Domain.BankAccount.ValueObject.AccountId (unwrapAccountId)
import Domain.DomainEventStore
import Domain.Event (parseEventData)
import GHC.Exception (toException)

instance (DomainEventStore m, MonadIO m) => AddressRepository m where
  findById accountId = do
    let aggregateType = (aggregateTypeToText Account)
        eventNames = [AddressUpserted.eventName]
        aggregateId = UUID.toText (unwrapAccountId accountId)

    eventsResult <- getLatestEventsByAggregateAndEventNames aggregateId aggregateType eventNames

    case eventsResult of
      Left err -> return $ Left err
      Right [] -> return $ Right Nothing
      Right (event : _) ->
        case parseEventData event >>= parseAddressFromEvent of
          Nothing ->
            return $ Left (toException (userError "Failed to parse entity rom event data"))
          Just validEntity ->
            return $ Right (Just validEntity)

  save _ = do
    return $ Right ()
