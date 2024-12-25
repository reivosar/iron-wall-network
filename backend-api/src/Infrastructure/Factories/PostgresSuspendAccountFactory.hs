module Infrastructure.Factories.PostgresSuspendAccountFactory (createSuspendAccount) where

import Application.BankAccount.Factories.SuspendAccountFactory
import Data.Text (Text)
import Data.Time.Clock (UTCTime)
import Data.UUID (UUID)
import Domain.BankAccount.Entity.SuspendAccount
  ( SuspendAccount,
    mkSuspendAccount,
  )
import Domain.BankAccount.ValueObject.AccountId (mkAccountId)
import Domain.ValueError (ValueError)

instance SuspendAccountFactory IO where
  createSuspendAccount uuid suspendedAt suspensionReason = do
    let accountId = mkAccountId uuid
    pure $ Right $ mkSuspendAccount accountId suspendedAt suspensionReason
