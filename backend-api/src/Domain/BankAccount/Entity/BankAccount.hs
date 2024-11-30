module Domain.BankAccount.Entity.BankAccount (BankAccount (..), mkBankAccount, accountCreated) where

import Data.Text (Text)
import Data.Time.Clock (UTCTime)
import qualified Domain.BankAccount.Events.AccountCreated as Event
import Domain.BankAccount.ValueObject.AccountId (AccountId, unwrapAccountId)
import Domain.BankAccount.ValueObject.Email (Email, unwrapEmail)
import Domain.BankAccount.ValueObject.FullName (FullName, unwrapFullName)
import Domain.BankAccount.ValueObject.Username (Username, unwrapUsername)

data BankAccount = BankAccount
  { accountId :: AccountId,
    username :: Username,
    fullName :: FullName,
    email :: Email,
    createdAt :: UTCTime
  }
  deriving (Show, Eq)

mkBankAccount :: AccountId -> Username -> FullName -> Email -> UTCTime -> BankAccount
mkBankAccount accId uname fname mail createdAt =
  BankAccount
    { accountId = accId,
      username = uname,
      fullName = fname,
      email = mail,
      createdAt = createdAt
    }

accountCreated :: BankAccount -> Event.AccountCreated
accountCreated entity =
  Event.AccountCreated
    { Event.accountId = unwrapAccountId (accountId entity),
      Event.username = unwrapUsername (username entity),
      Event.fullName = unwrapFullName (fullName entity),
      Event.email = unwrapEmail (email entity),
      Event.createdAt = createdAt entity
    }
