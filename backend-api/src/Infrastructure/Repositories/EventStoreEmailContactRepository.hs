{-# LANGUAGE FlexibleContexts #-}

module Infrastructure.Repositories.EventStoreEmailContactRepository (findById, save) where

import Domain.BankAccount.Repositories.EmailContactRepository

instance EmailContactRepository IO where
  findById _ = do
    return $ Right Nothing

  save _ = do
    return $ Right ()
