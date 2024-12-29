module Domain.BankAccount.Entity.PhoneNumberContact
  ( PhoneNumberContact (..),
    mkPhoneNumberContact,
    changePhoneNumber,
    phoneNumberUpserted,
  )
where

import Data.Text ()
import Data.Time (UTCTime)
import qualified Domain.BankAccount.Events.PhoneNumberContactUpserted as Event
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

data PhoneNumberContact = PhoneNumberContact
  { accountId :: AccountId,
    phoneType :: PhoneType,
    phoneNumber :: PhoneNumber
  }
  deriving (Show, Eq)

mkPhoneNumberContact :: AccountId -> PhoneType -> PhoneNumber -> PhoneNumberContact
mkPhoneNumberContact accId phnType number =
  PhoneNumberContact
    { accountId = accId,
      phoneType = phnType,
      phoneNumber = number
    }

changePhoneNumber :: PhoneNumberContact -> PhoneNumber -> PhoneType -> PhoneNumberContact
changePhoneNumber phnNmbrCntct phnNmbr phnTyp =
  mkPhoneNumberContact
    (accountId phnNmbrCntct)
    phnTyp
    phnNmbr

phoneNumberUpserted :: PhoneNumberContact -> UTCTime -> Event.PhoneNumberContactUpserted
phoneNumberUpserted phone timestamp =
  Event.PhoneNumberContactUpserted
    { Event.accountId = unwrapAccountId (accountId phone),
      Event.phoneNumber = unwrapPhoneNumber (phoneNumber phone),
      Event.phoneType = phoneTypeToText (phoneType phone),
      Event.updatedAt = timestamp
    }
