{-# LANGUAGE FlexibleContexts #-}

module Domain.BankAccount.Repositories.EmergencyContactRepository (EmergencyContactRepository (..)) where

import Control.Exception (SomeException)
import Domain.BankAccount.Entity.EmergencyContact (EmergencyContact)
import Domain.BankAccount.ValueObject.AccountId (AccountId)

class EmergencyContactRepository m where
  findEmergencyContactById :: AccountId -> m (Either SomeException (Maybe EmergencyContact))
  save :: EmergencyContact -> m (Either SomeException ())
