{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}

module Infrastructure.Services.BankAccount.EventStoreBankAccountService
  ( tryCreate,
    tryApprove,
    tryActivate,
    tryPend,
    trySuspend,
    tryClose,
  )
where

import Control.Exception (SomeException)
import Control.Monad.IO.Class (MonadIO)
import Data.List (find)
import Data.Text (Text, pack)
import qualified Data.UUID as UUID
import Domain.AggregateId (mkAggregateId)
import Domain.AggregateType (AggregateType (..))
import qualified Domain.BankAccount.Events.AccountActivated as AccountActivated
import qualified Domain.BankAccount.Events.AccountApproved as AccountApproved
import qualified Domain.BankAccount.Events.AccountClosed as AccountClosed
import qualified Domain.BankAccount.Events.AccountCreated as AccountCreated
import qualified Domain.BankAccount.Events.AccountPended as AccountPended
import qualified Domain.BankAccount.Events.AccountSuspended as AccountSuspended
import Domain.BankAccount.Services.BankAccountService
import Domain.BankAccount.ValueObject.AccountId
import Domain.DomainEventStore
import Domain.Error (DomainError, mkDomainError)
import qualified Domain.Event as DE
import Domain.Shared.Services.EventStatusValidator (EventStatusValidator, validateEventStatus)

instance (DomainEventStore m, EventStatusValidator m) => BankAccountService m where
  tryCreate accId = do
    return $ Right ()

  tryApprove accId = do
    validateEventStatus
      (mkAggregateId (pack $ UUID.toString $ unwrapAccountId accId))
      Account
      [AccountCreated.eventName]
      [AccountApproved.eventName]

  tryActivate accId = do
    validateEventStatus
      (mkAggregateId (pack $ UUID.toString $ unwrapAccountId accId))
      Account
      [AccountCreated.eventName, AccountApproved.eventName]
      [AccountActivated.eventName]

  tryPend accId = do
    validateEventStatus
      (mkAggregateId (pack $ UUID.toString $ unwrapAccountId accId))
      Account
      [AccountCreated.eventName]
      [AccountSuspended.eventName, AccountPended.eventName]

  trySuspend accId = do
    validateEventStatus
      (mkAggregateId (pack $ UUID.toString $ unwrapAccountId accId))
      Account
      [AccountCreated.eventName, AccountApproved.eventName]
      [AccountSuspended.eventName]

  tryClose accId = do
    validateEventStatus
      (mkAggregateId (pack $ UUID.toString $ unwrapAccountId accId))
      Account
      [AccountCreated.eventName, AccountApproved.eventName]
      [AccountClosed.eventName]
