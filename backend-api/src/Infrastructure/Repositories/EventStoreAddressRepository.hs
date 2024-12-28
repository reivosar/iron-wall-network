{-# LANGUAGE FlexibleContexts #-}

module Infrastructure.Repositories.EventStoreAddressRepository (findById, save) where

import Domain.BankAccount.Repositories.AddressRepository

instance AddressRepository IO where
  findById _ = do
    return $ Right Nothing

  save _ = do
    return $ Right ()
