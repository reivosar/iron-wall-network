{-# LANGUAGE FlexibleContexts #-}

module Domain.BankAccount.Repositories.AccountRepository (AccountRepository (..)) where

import Control.Exception (SomeException)
import Domain.BankAccount.ValueObject.AccountId (AccountId)
import Utils.UUIDGenerator (generateUUID)

class AccountRepository m where
  generateAccountId :: m (AccountId)
