{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UndecidableInstances #-}

module Infrastructure.Repositories.BankAccount.EventStoreFundsRepository (findById, save) where

import Control.Monad.IO.Class (MonadIO)
import Data.Function (on)
import Data.Maybe (fromJust)
import Data.Text (unpack)
import qualified Data.UUID as UUID
import Domain.AggregateType (AggregateType (..), aggregateTypeToText)
import Domain.BankAccount.Entity.ActiveAccount (ActiveAccount)
import Domain.BankAccount.Entity.Funds (Funds, mkFunds, parseFundsFromDepositedEvent, parseFundsFromWithdrawnEvent)
import qualified Domain.BankAccount.Events.AccountActivated as AccountActivated
import qualified Domain.BankAccount.Events.FundsDeposited as FundsDeposited
import qualified Domain.BankAccount.Events.FundsWithdrawn as FundsWithdrawn
import Domain.BankAccount.Repositories.FundsRepository
import Domain.BankAccount.ValueObject.AccountId (mkAccountId, unwrapAccountId)
import Domain.BankAccount.ValueObject.Balance (mkBalance)
import Domain.DomainEventStore
import Domain.Event (Event (eventType, sequenceNumber), parseEventData)
import GHC.Exception (SomeException, toException)
import Utils.Conversions (maximumByMay)

instance (DomainEventStore m, MonadIO m) => FundsRepository m where
  findById accountId = do
    let aggregateType = aggregateTypeToText Account
        eventNames = [AccountActivated.eventName, FundsDeposited.eventName, FundsWithdrawn.eventName]
        aggregateId = UUID.toText (unwrapAccountId accountId)

    eventsResult <- getLatestEventsByAggregateAndEventNames aggregateId aggregateType eventNames

    case eventsResult of
      Left err -> return $ Left err
      Right [] -> return $ Right Nothing
      Right events ->
        case maximumByMay (compare `on` sequenceNumber) events of
          Nothing -> return $ Left (toException (userError "No latest event found"))
          Just latestEvent -> handleLatestEvent latestEvent

  save _ = do
    return $ Right ()

handleLatestEvent :: (MonadIO m) => Event -> m (Either SomeException (Maybe Funds))
handleLatestEvent latestEvent =
  case eventType latestEvent of
    et | et == FundsDeposited.eventName -> handleDepositedEvent latestEvent
    et | et == FundsWithdrawn.eventName -> handleWithdrawnEvent latestEvent
    _ -> handleAccountActivated latestEvent

handleDepositedEvent :: (MonadIO m) => Event -> m (Either SomeException (Maybe Funds))
handleDepositedEvent event =
  case parseEventData event >>= parseFundsFromDepositedEvent of
    Nothing -> return $ Left (toException (userError "Failed to parse deposited event data"))
    Just funds -> return $ Right (Just funds)

handleWithdrawnEvent :: (MonadIO m) => Event -> m (Either SomeException (Maybe Funds))
handleWithdrawnEvent event =
  case parseEventData event >>= parseFundsFromWithdrawnEvent of
    Nothing -> return $ Left (toException (userError "Failed to parse withdrawn event data"))
    Just funds -> return $ Right (Just funds)

handleAccountActivated :: (MonadIO m) => Event -> m (Either SomeException (Maybe Funds))
handleAccountActivated event = do
  let activeAccount = parseEventData event
      balanceResult = mkBalance 0
  case activeAccount of
    Nothing -> return $ Left (toException (userError "Invalid account data"))
    Just acc ->
      case balanceResult of
        Left _ -> return $ Left (toException (userError "Failed to create balance"))
        Right bal ->
          case mkFunds (mkAccountId $ AccountActivated.accountId acc) bal of
            Left err -> return $ Left (toException $ userError $ show err)
            Right funds -> return $ Right $ Just funds
