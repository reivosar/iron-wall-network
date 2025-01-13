{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module Infrastructure.Repositories.EventStorePhoneNumberRepository (findById, save) where

import Control.Monad.IO.Class (MonadIO)
import Domain.BankAccount.Repositories.PhoneNumberRepository
import Domain.DomainEventStore

instance (DomainEventStore m, MonadIO m) => PhoneNumberRepository m where
  findById _ = do
    return $ Right Nothing

  save _ = do
    return $ Right ()
