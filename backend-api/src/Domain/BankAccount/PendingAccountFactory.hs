module Domain.BankAccount.PendingAccountFactory (createPendingAccount) where

import Data.Time.Clock (UTCTime)
import Data.UUID (UUID)
import Domain.BankAccount.Entity.PendingAccount
  ( PendingAccount,
    mkPendingAccount,
  )
import Domain.BankAccount.ValueObject.AccountId (mkAccountId)
import Domain.ValueError (ValueError)

createPendingAccount ::
  UUID ->
  UTCTime ->
  Either ValueError PendingAccount
createPendingAccount uuid pendedAt = do
  accountId <- mkAccountId uuid
  Right $ mkPendingAccount accountId pendedAt
