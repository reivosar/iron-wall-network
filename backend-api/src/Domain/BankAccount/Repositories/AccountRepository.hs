{-# LANGUAGE FlexibleContexts #-}

module Domain.BankAccount.Repositories.AccountRepository (AccountRepository (..)) where

import Domain.BankAccount.ValueObject.AccountId (AccountId)

class AccountRepository m where
  generateAccountId :: m (AccountId)
