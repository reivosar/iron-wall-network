module Domain.BankAccount.Entity.PendingAccount
  ( PendingAccount,
    mkPendingAccount,
    accountPended,
  )
where

import Data.Text (Text)
import Data.Time.Clock (UTCTime)
import qualified Domain.BankAccount.Events.AccountPended as Event
import Domain.BankAccount.ValueObject.AccountId
  ( AccountId,
    unwrapAccountId,
  )

data PendingAccount = PendingAccount
  { accountId :: AccountId,
    pendedAt :: UTCTime,
    pendedReason :: Maybe Text
  }
  deriving (Show, Eq)

mkPendingAccount :: AccountId -> UTCTime -> Maybe Text -> PendingAccount
mkPendingAccount accId pendAt reason =
  PendingAccount
    { accountId = accId,
      pendedAt = pendAt,
      pendedReason = reason
    }

accountPended :: PendingAccount -> Event.AccountPended
accountPended entity =
  Event.AccountPended
    { Event.accountId = unwrapAccountId (accountId entity),
      Event.reason = pendedReason entity,
      Event.pendedAt = pendedAt entity
    }
