{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module Infrastructure.Repositories.EventStoreEmailContactRepository (findById, save) where

import Control.Monad.IO.Class (MonadIO)
import Domain.BankAccount.Repositories.EmailContactRepository
import Domain.DomainEventStore

instance (DomainEventStore m, MonadIO m) => EmailContactRepository m where
  findById _ = do
    return $ Right Nothing

  save _ = do
    return $ Right ()
