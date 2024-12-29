{-# LANGUAGE FlexibleContexts #-}

module Infrastructure.Repositories.EventStoreEmergencyContactRepository (findById, save) where

import Domain.BankAccount.Repositories.EmergencyContactRepository

instance EmergencyContactRepository IO where
  findById _ = do
    return $ Right Nothing

  save _ = do
    return $ Right ()
