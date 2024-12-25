module Infrastructure.Factories.PostgresApproveAccountFactory (createApproveAccount) where

import Application.BankAccount.Factories.ApproveAccountFactory
import Data.Text (Text)
import Data.Time.Clock (UTCTime)
import Data.UUID (UUID)
import Domain.BankAccount.Entity.ApproveAccount
  ( ApproveAccount,
    mkApproveAccount,
  )
import Domain.BankAccount.ValueObject.AccountId (mkAccountId)
import Domain.ValueError (ValueError)

instance ApproveAccountFactory IO where
  createApproveAccount uuid approvedAt approvalNotes = do
    let accountId = mkAccountId uuid
    pure $ Right $ mkApproveAccount accountId approvedAt approvalNotes
