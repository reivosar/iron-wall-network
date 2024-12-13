module Domain.BankAccount.CloseAccountFactory (createCloseAccount) where

import Data.Text (Text)
import Data.Time.Clock (UTCTime)
import Data.UUID (UUID)
import Domain.BankAccount.Entity.CloseAccount
  ( CloseAccount,
    mkCloseAccount,
  )
import Domain.BankAccount.ValueObject.AccountId (mkAccountId)
import Domain.ValueError (ValueError)

createCloseAccount ::
  UUID ->
  UTCTime ->
  Maybe Text ->
  Either ValueError CloseAccount
createCloseAccount uuid closedAt closureReason = do
  accountId <- mkAccountId uuid
  Right $ mkCloseAccount accountId closedAt closureReason
