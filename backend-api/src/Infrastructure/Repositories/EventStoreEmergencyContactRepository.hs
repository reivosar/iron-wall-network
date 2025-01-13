{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module Infrastructure.Repositories.EventStoreEmergencyContactRepository (findById, save) where

import Control.Monad.IO.Class (MonadIO)
import Domain.BankAccount.Repositories.EmergencyContactRepository
import Domain.DomainEventStore

instance (DomainEventStore m, MonadIO m) => EmergencyContactRepository m where
  findById _ = do
    return $ Right Nothing

  save _ = do
    return $ Right ()
