module Domain.BankAccount.Entity.PendingAccount (PendingAccount, mkPendingAccount, accountPended) where

import Data.Time.Clock (UTCTime)
import qualified Domain.BankAccount.Events.AccountPended as Event
import Domain.BankAccount.ValueObject.AccountId (AccountId, unwrapAccountId)

data PendingAccount = PendingAccount
  { accountId :: AccountId,
    pendedAt :: UTCTime
  }
  deriving (Show, Eq)

mkPendingAccount :: AccountId -> UTCTime -> PendingAccount
mkPendingAccount accId pendedAt =
  PendingAccount
    { accountId = accId,
      pendedAt = pendedAt
    }

accountPended :: PendingAccount -> Event.AccountPended
accountPended entity =
  Event.AccountPended
    { Event.accountId = unwrapAccountId (accountId entity),
      Event.pendedAt = pendedAt entity
    }
