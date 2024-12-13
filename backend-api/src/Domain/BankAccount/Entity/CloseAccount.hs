module Domain.BankAccount.Entity.CloseAccount (CloseAccount, mkCloseAccount, accountClosed) where

import Data.Text (Text)
import Data.Time.Clock (UTCTime)
import qualified Domain.BankAccount.Events.AccountClosed as Event
import Domain.BankAccount.ValueObject.AccountId
  ( AccountId,
    unwrapAccountId,
  )

data CloseAccount = CloseAccount
  { accountId :: AccountId,
    closedAt :: UTCTime,
    closureReason :: Maybe Text
  }
  deriving (Show, Eq)

mkCloseAccount :: AccountId -> UTCTime -> Maybe Text -> CloseAccount
mkCloseAccount accId closedAt closureReason =
  CloseAccount
    { accountId = accId,
      closedAt = closedAt,
      closureReason = closureReason
    }

accountClosed :: CloseAccount -> Event.AccountClosed
accountClosed entity =
  Event.AccountClosed
    { Event.accountId = unwrapAccountId (accountId entity),
      Event.closedAt = closedAt entity,
      Event.reason = closureReason entity
    }
