module Infrastructure.Factories.PostgresApproveAccountFactory (createApproveAccount) where

import Application.BankAccount.Factories.ApproveAccountFactory
import Domain.BankAccount.Entity.ApproveAccount
  ( mkApproveAccount,
  )
import Domain.BankAccount.ValueObject.AccountId (mkAccountId)

instance ApproveAccountFactory IO where
  createApproveAccount uuid approvedAt approvalNotes = do
    let accountId = mkAccountId uuid
    pure $ Right $ mkApproveAccount accountId approvedAt approvalNotes
