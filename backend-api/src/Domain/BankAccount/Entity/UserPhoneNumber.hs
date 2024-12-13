module Domain.BankAccount.Entity.UserPhoneNumber
  ( PhoneNumber (..),
    mkUserPhoneNumber,
    changePhoneNumber,
    phoneNumberUpserted,
  )
where

import Data.Text ()
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
import Domain.BankAccount.ValueObject.PhoneType
  ( PhoneType,
    phoneTypeToText,
  )

data UserPhoneNumber = UserPhoneNumber
  { accountId :: AccountId,
    phoneType :: PhoneType,
    phoneNumber :: PhoneNumber
  }
  deriving (Show, Eq)

mkUserPhoneNumber :: AccountId -> PhoneType -> PhoneNumber -> UserPhoneNumber
mkUserPhoneNumber accId phnType number =
  UserPhoneNumber
    { accountId = accId,
      phoneType = phnType,
      phoneNumber = number
    }

changePhoneNumber :: UserPhoneNumber -> PhoneNumber -> UserPhoneNumber
changePhoneNumber userPhoneNumber phnNmbr =
  mkUserPhoneNumber
    (accountId userPhoneNumber)
    (phoneType userPhoneNumber)
    phnNmbr

phoneNumberUpserted :: UserPhoneNumber -> UTCTime -> Event.PhoneNumberUpserted
phoneNumberUpserted phone timestamp =
  Event.PhoneNumberUpserted
    { Event.accountId = unwrapAccountId (accountId phone),
      Event.phoneNumber = unwrapPhoneNumber (phoneNumber phone),
      Event.phoneType = phoneTypeToText (phoneType phone),
      Event.updatedAt = timestamp
    }
