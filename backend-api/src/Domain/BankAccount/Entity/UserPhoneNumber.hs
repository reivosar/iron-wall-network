module Domain.BankAccount.Entity.UserPhoneNumber (PhoneNumber (..), changePhoneNumber, phoneNumberUpserted) where

import Data.Text (Text)
import Data.Time (UTCTime)
import qualified Domain.BankAccount.Events.PhoneNumberUpserted as Event
import Domain.BankAccount.ValueObject.AccountId
  ( AccountId,
    unwrapAccountId,
  )
import Domain.BankAccount.ValueObject.PhoneNumber
  ( PhoneNumber,
    unwrapPhoneNumber,
  )

data UserPhoneNumber = UserPhoneNumber
  { accountId :: AccountId,
    phoneNumber :: PhoneNumber
  }
  deriving (Show, Eq)

mkUserPhoneNumber :: AccountId -> PhoneNumber -> UserPhoneNumber
mkUserPhoneNumber accId phone =
  UserPhoneNumber
    { accountId = accId,
      phoneNumber = phone
    }

changePhoneNumber :: UserPhoneNumber -> PhoneNumber -> UserPhoneNumber
changePhoneNumber userPhoneNumber phoneNumber = userPhoneNumber {phoneNumber = phoneNumber}

phoneNumberUpserted :: UserPhoneNumber -> UTCTime -> Event.PhoneNumberUpserted
phoneNumberUpserted phone timestamp =
  Event.PhoneNumberUpserted
    { Event.accountId = unwrapAccountId (accountId phone),
      Event.phoneNumber = unwrapPhoneNumber (phoneNumber phone),
      Event.updatedAt = timestamp
    }
