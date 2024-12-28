{-# LANGUAGE FlexibleContexts #-}

module Domain.BankAccount.Repositories.PhoneNumberRepository (PhoneNumberRepository (..)) where

import Control.Exception (SomeException)
import Domain.BankAccount.Entity.PhoneNumberContact (PhoneNumberContact)
import Domain.BankAccount.ValueObject.AccountId (AccountId)

class PhoneNumberRepository m where
  findPhoneNumberById :: AccountId -> m (Either SomeException (Maybe PhoneNumberContact))
  save :: PhoneNumberContact -> m (Either SomeException ())
