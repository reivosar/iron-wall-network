module Domain.BankAccount.SuspendAccountFactory (createSuspendAccount) where

import Data.Text (Text)
import Data.Time.Clock (UTCTime)
import Data.UUID (UUID)
import Domain.BankAccount.Entity.SuspendAccount
  ( SuspendAccount,
    mkSuspendAccount,
  )
import Domain.BankAccount.ValueObject.AccountId (mkAccountId)
import Domain.ValueError (ValueError)

createSuspendAccount ::
  UUID ->
  UTCTime ->
  Maybe Text ->
  Either ValueError SuspendAccount
createSuspendAccount uuid suspendedAt suspensionReason = do
  accountId <- mkAccountId uuid
  Right $ mkSuspendAccount accountId suspendedAt suspensionReason
