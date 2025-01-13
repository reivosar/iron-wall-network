{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}

module Domain.BankAccount.Services.BankAccountService (BankAccountService (..)) where

import Data.Aeson (ToJSON)
import Data.Text (Text)
import Domain.BankAccount.ValueObject.AccountId (AccountId)
import Domain.Error (DomainError)
import GHC.Generics (Generic)

class (Monad m) => BankAccountService m where
  tryCreate :: AccountId -> m (Either DomainError ())
  tryApprove :: AccountId -> m (Either DomainError ())
  tryActivate :: AccountId -> m (Either DomainError ())
  tryPend :: AccountId -> m (Either DomainError ())
  trySuspend :: AccountId -> m (Either DomainError ())
  tryClose :: AccountId -> m (Either DomainError ())
