module Domain.BankAccount.Entity.ActiveAccount
  ( ActiveAccount,
    mkActiveAccount,
    accountActivated,
  )
where

import Data.Time.Clock (UTCTime)
import qualified Domain.BankAccount.Events.AccountActivated as Event
import Domain.BankAccount.ValueObject.AccountId
  ( AccountId,
    unwrapAccountId,
  )
import Domain.BankAccount.ValueObject.AccountPassword
  ( AccountPassword,
    unwrapPasswordHash,
  )

data ActiveAccount = ActiveAccount
  { accountId :: AccountId,
    password :: AccountPassword,
    activatedAt :: UTCTime
  }
  deriving (Show, Eq)

mkActiveAccount :: AccountId -> AccountPassword -> UTCTime -> ActiveAccount
mkActiveAccount accId psswrd activtdAt =
  ActiveAccount
    { accountId = accId,
      password = psswrd,
      activatedAt = activtdAt
    }

accountActivated :: ActiveAccount -> Event.AccountActivated
accountActivated entity =
  Event.AccountActivated
    { Event.accountId = unwrapAccountId (accountId entity),
      Event.password = unwrapPasswordHash (password entity),
      Event.activatedAt = activatedAt entity
    }
