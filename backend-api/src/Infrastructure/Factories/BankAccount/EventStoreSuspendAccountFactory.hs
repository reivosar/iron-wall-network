{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module Infrastructure.Factories.BankAccount.EventStoreSuspendAccountFactory (createSuspendAccount) where

import Application.BankAccount.Factories.SuspendAccountFactory
import Domain.BankAccount.Entity.SuspendAccount (mkSuspendAccount)
import Domain.BankAccount.Services.BankAccountService (BankAccountService, trySuspend)
import Domain.BankAccount.ValueObject.AccountId (mkAccountId)

instance (BankAccountService m) => SuspendAccountFactory m where
  createSuspendAccount uuid suspendedAt suspensionReason = do
    let accountId = mkAccountId uuid
    suspensionCheck <- trySuspend accountId
    case suspensionCheck of
      Left err -> return $ Left err
      Right _ -> do
        return $ Right $ mkSuspendAccount accountId suspendedAt suspensionReason
