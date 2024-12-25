module Infrastructure.Factories.PostgresCloseAccountFactory (createCloseAccount) where

import Application.BankAccount.Factories.CloseAccountFactory
import Data.Text (Text)
import Data.Time.Clock (UTCTime)
import Data.UUID (UUID)
import Domain.BankAccount.Entity.CloseAccount
  ( CloseAccount,
    mkCloseAccount,
  )
import Domain.BankAccount.ValueObject.AccountId (mkAccountId)
import Domain.ValueError (ValueError)

instance CloseAccountFactory IO where
  createCloseAccount uuid closedAt closureReason = do
    let accountId = mkAccountId uuid
    pure $ Right $ mkCloseAccount accountId closedAt closureReason
