{-# LANGUAGE FlexibleContexts #-}

module Domain.BankAccount.Repositories.AddressRepository (AddressRepository (..)) where

import Control.Exception (SomeException)
import Domain.BankAccount.Entity.Address (Address)
import Domain.BankAccount.ValueObject.AccountId (AccountId)

class AddressRepository m where
  findAddressById :: AccountId -> m (Either SomeException (Maybe Address))
  save :: Address -> m (Either SomeException ())
