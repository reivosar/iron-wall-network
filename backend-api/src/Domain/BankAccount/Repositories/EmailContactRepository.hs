{-# LANGUAGE FlexibleContexts #-}

module Domain.BankAccount.Repositories.EmailContactRepository (EmailContactRepository (..)) where

import Control.Exception (SomeException)
import Domain.BankAccount.Entity.EmailContact (EmailContact)
import Domain.BankAccount.ValueObject.AccountId (AccountId)

class EmailContactRepository m where
  findById :: AccountId -> m (Either SomeException (Maybe EmailContact))
  save :: EmailContact -> m (Either SomeException ())
