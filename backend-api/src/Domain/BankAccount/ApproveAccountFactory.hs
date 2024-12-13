module Domain.BankAccount.ApproveAccountFactory (createApproveAccount) where

import Data.Text (Text)
import Data.Time.Clock (UTCTime)
import Data.UUID (UUID)
import Domain.BankAccount.Entity.ApproveAccount
  ( ApproveAccount,
    mkApproveAccount,
  )
import Domain.BankAccount.ValueObject.AccountId (mkAccountId)
import Domain.ValueError (ValueError)

createApproveAccount ::
  UUID ->
  UTCTime ->
  Maybe Text ->
  Either ValueError ApproveAccount
createApproveAccount uuid approvedAt approvalNotes = do
  accountId <- mkAccountId uuid
  Right $ mkApproveAccount accountId approvedAt approvalNotes
