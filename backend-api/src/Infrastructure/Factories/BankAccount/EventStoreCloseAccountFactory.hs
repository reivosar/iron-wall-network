{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module Infrastructure.Factories.BankAccount.EventStoreCloseAccountFactory (createCloseAccount) where

import Application.BankAccount.Factories.CloseAccountFactory
import Control.Monad.IO.Class (MonadIO)
import Domain.BankAccount.Entity.CloseAccount (mkCloseAccount)
import Domain.BankAccount.Services.BankAccountService (BankAccountService, tryClose)
import Domain.BankAccount.ValueObject.AccountId (mkAccountId)

instance (BankAccountService m, MonadIO m) => CloseAccountFactory m where
  createCloseAccount uuid closedAt closureReason = do
    let accountId = mkAccountId uuid
    closeCheck <- tryClose accountId
    case closeCheck of
      Left err -> return $ Left err
      Right _ -> do
        return $ Right $ mkCloseAccount accountId closedAt closureReason
