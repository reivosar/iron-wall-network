{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}

module Domain.BankAccount.Services.BankAccountService (BankAccountService (..)) where

import Domain.BankAccount.ValueObject.AccountId (AccountId)
import Domain.BankAccount.ValueObject.Username (Username)
import Domain.Error (DomainError)

class (Monad m) => BankAccountService m where
  tryCreate :: Username -> m (Either DomainError ())
  tryApprove :: AccountId -> m (Either DomainError ())
  tryActivate :: AccountId -> m (Either DomainError ())
  tryPend :: AccountId -> m (Either DomainError ())
  trySuspend :: AccountId -> m (Either DomainError ())
  tryClose :: AccountId -> m (Either DomainError ())
