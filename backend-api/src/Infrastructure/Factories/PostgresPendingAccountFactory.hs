module Infrastructure.Factories.PostgresPendingAccountFactory (createPendingAccount) where

import Application.BankAccount.Factories.PendingAccountFactory
import Data.Text (Text)
import Data.Time.Clock (UTCTime)
import Data.UUID (UUID)
import Domain.BankAccount.Entity.PendingAccount
  ( PendingAccount,
    mkPendingAccount,
  )
import Domain.BankAccount.ValueObject.AccountId (mkAccountId)
import Domain.ValueError (ValueError)

instance PendingAccountFactory IO where
  createPendingAccount uuid pendedAt reason = do
    let accountId = mkAccountId uuid
    pure $ Right $ mkPendingAccount accountId pendedAt reason
