{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module Infrastructure.Repositories.EventStoreAddressRepository (findById, save) where

import Control.Monad.IO.Class (MonadIO)
import Domain.BankAccount.Repositories.AddressRepository
import Domain.DomainEventStore

instance (DomainEventStore m, MonadIO m) => AddressRepository m where
  findById _ = do
    return $ Right Nothing

  save _ = do
    return $ Right ()
