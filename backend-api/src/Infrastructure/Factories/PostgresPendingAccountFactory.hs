module Infrastructure.Factories.PostgresPendingAccountFactory (createPendingAccount) where

import Application.BankAccount.Factories.PendingAccountFactory
import Domain.BankAccount.Entity.PendingAccount
  ( mkPendingAccount,
  )
import Domain.BankAccount.ValueObject.AccountId (mkAccountId)

instance PendingAccountFactory IO where
  createPendingAccount uuid pendedAt reason = do
    let accountId = mkAccountId uuid
    pure $ Right $ mkPendingAccount accountId pendedAt reason
