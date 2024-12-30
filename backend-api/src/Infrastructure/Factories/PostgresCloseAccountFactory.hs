module Infrastructure.Factories.PostgresCloseAccountFactory (createCloseAccount) where

import Application.BankAccount.Factories.CloseAccountFactory
import Domain.BankAccount.Entity.CloseAccount
  ( mkCloseAccount,
  )
import Domain.BankAccount.ValueObject.AccountId (mkAccountId)

instance CloseAccountFactory IO where
  createCloseAccount uuid closedAt closureReason = do
    let accountId = mkAccountId uuid
    pure $ Right $ mkCloseAccount accountId closedAt closureReason
