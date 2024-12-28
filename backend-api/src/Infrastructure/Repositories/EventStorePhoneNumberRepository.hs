{-# LANGUAGE FlexibleContexts #-}

module Infrastructure.Repositories.EventStorePhoneNumberRepository (findById, save) where

import Domain.BankAccount.Repositories.PhoneNumberRepository

instance PhoneNumberRepository IO where
  findById _ = do
    return $ Right Nothing

  save _ = do
    return $ Right ()
