module Infrastructure.Factories.PostgresSuspendAccountFactory (createSuspendAccount) where

import Application.BankAccount.Factories.SuspendAccountFactory
import Domain.BankAccount.Entity.SuspendAccount
  ( mkSuspendAccount,
  )
import Domain.BankAccount.ValueObject.AccountId (mkAccountId)

instance SuspendAccountFactory IO where
  createSuspendAccount uuid suspendedAt suspensionReason = do
    let accountId = mkAccountId uuid
    pure $ Right $ mkSuspendAccount accountId suspendedAt suspensionReason
