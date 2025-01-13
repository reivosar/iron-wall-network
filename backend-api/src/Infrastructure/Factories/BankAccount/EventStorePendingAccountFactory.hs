{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module Infrastructure.Factories.BankAccount.EventStorePendingAccountFactory (createPendingAccount) where

import Application.BankAccount.Factories.PendingAccountFactory
import Control.Monad.IO.Class (MonadIO)
import Domain.BankAccount.Entity.PendingAccount (mkPendingAccount)
import Domain.BankAccount.Services.BankAccountService (BankAccountService, tryPend)
import Domain.BankAccount.ValueObject.AccountId (mkAccountId)

instance (BankAccountService m, MonadIO m) => PendingAccountFactory m where
  createPendingAccount uuid pendedAt reason = do
    let accountId = mkAccountId uuid
    pendingCheck <- tryPend accountId
    case pendingCheck of
      Left err -> return $ Left err
      Right _ -> do
        return $ Right $ mkPendingAccount accountId pendedAt reason
