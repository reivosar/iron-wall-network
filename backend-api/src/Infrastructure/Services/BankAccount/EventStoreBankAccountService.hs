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

import Control.Monad.IO.Class (MonadIO)
import Domain.BankAccount.Services.BankAccountService
import Domain.BankAccount.ValueObject.AccountId
import Domain.DomainEventStore
import GHC.Exception (toException)

instance (DomainEventStore m, MonadIO m) => BankAccountService m where
  tryCreate accId = do
    return $ Right ()

  tryApprove accId = do
    return $ Right ()

  tryActivate accId = do
    return $ Right ()

  tryPend accId = do
    return $ Right ()

  trySuspend accId = do
    return $ Right ()

  tryClose accId = do
    return $ Right ()

-- handleApproval :: (Monad m) => [Event] -> AccountId -> UTCTime -> Maybe Text -> m (Either SomeException ApproveAccount)
-- handleApproval events accountId approvedAt approvalNotes
--   | hasActivatedEvent events = return $ Left $ toException (userError "The account has already been approved or activated.")
--   | not (hasCreatedEvent events) = return $ Left $ toException (userError "The account creation event is missing. Approval cannot proceed.")
--   | otherwise = return $ Right $ mkApproveAccount accountId approvedAt approvalNotes

-- hasActivatedEvent :: [Event] -> Bool
-- hasActivatedEvent = any (\event -> eventType event == AccountActivated.eventName)

-- hasCreatedEvent :: [Event] -> Bool
-- hasCreatedEvent = any (\event -> eventType event == AccountCreated.eventName)
