{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module Infrastructure.Factories.BankAccount.EventStoreApproveAccountFactory (createApproveAccount) where

import Application.BankAccount.Factories.ApproveAccountFactory
import Domain.BankAccount.Entity.ApproveAccount (mkApproveAccount)
import Domain.BankAccount.Services.BankAccountService (BankAccountService, tryApprove)
import Domain.BankAccount.ValueObject.AccountId (mkAccountId)
import Infrastructure.Services.Shared.EventStoreEventStatusValidator

instance (BankAccountService m) => ApproveAccountFactory m where
  createApproveAccount uuid approvedAt approvalNotes = do
    let accountId = mkAccountId uuid
    approvalCheck <- tryApprove accountId
    case approvalCheck of
      Left err -> return $ Left err
      Right _ -> return $ Right $ mkApproveAccount accountId approvedAt approvalNotes
