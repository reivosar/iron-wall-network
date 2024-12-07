module Domain.BankAccount.Entity.SuspendAccount (SuspendAccount, mkSuspendAccount, accountSuspended) where

import Data.Text (Text)
import Data.Time.Clock (UTCTime)
import qualified Domain.BankAccount.Events.AccountSuspended as Event
import Domain.BankAccount.ValueObject.AccountId (AccountId, unwrapAccountId)

data SuspendAccount = SuspendAccount
  { accountId :: AccountId,
    suspendedAt :: UTCTime,
    suspensionReason :: Maybe Text
  }
  deriving (Show, Eq)

mkSuspendAccount :: AccountId -> UTCTime -> Maybe Text -> SuspendAccount
mkSuspendAccount accId suspendedAt suspensionReason =
  SuspendAccount
    { accountId = accId,
      suspendedAt = suspendedAt,
      suspensionReason = suspensionReason
    }

accountSuspended :: SuspendAccount -> Event.AccountSuspended
accountSuspended entity =
  Event.AccountSuspended
    { Event.accountId = unwrapAccountId (accountId entity),
      Event.suspendedAt = suspendedAt entity,
      Event.reason = suspensionReason entity
    }
