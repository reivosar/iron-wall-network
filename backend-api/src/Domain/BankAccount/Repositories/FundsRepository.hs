{-# LANGUAGE FlexibleContexts #-}

module Domain.BankAccount.Repositories.FundsRepository (FundsRepository (..)) where

import Control.Exception (SomeException)
import Domain.BankAccount.Entity.Funds (Funds)
import Domain.BankAccount.ValueObject.AccountId (AccountId)

class FundsRepository m where
  findById :: AccountId -> m (Either SomeException (Maybe Funds))
  save :: Funds -> m (Either SomeException ())
