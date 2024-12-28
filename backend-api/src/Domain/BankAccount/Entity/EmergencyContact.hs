module Domain.BankAccount.Entity.EmergencyContact
  ( EmergencyContact (..),
    changeEmail,
    mkEmergencyContact,
    emailUpserted,
  )
where

import Data.Text ()
import Data.Time (UTCTime)
import qualified Domain.BankAccount.Events.EmergencyContactUpserted as Event
import Domain.BankAccount.ValueObject.AccountId
  ( AccountId,
    unwrapAccountId,
  )
import Domain.BankAccount.ValueObject.FullName
  ( FullName,
    unwrapFullName,
  )
import Domain.BankAccount.ValueObject.PhoneNumber
  ( PhoneNumber,
    unwrapPhoneNumber,
  )

data EmergencyContact = EmergencyContact
  { accountId :: AccountId,
    name :: FullName,
    phoneNumber :: PhoneNumber
  }
  deriving (Show, Eq)

mkEmergencyContact :: AccountId -> FullName -> PhoneNumber -> EmergencyContact
mkEmergencyContact accId fllNm phnNmbr =
  EmergencyContact
    { accountId = accId,
      name = fllNm,
      phoneNumber = phnNmbr
    }

changeEmail :: EmergencyContact -> FullName -> PhoneNumber -> EmergencyContact
changeEmail contact fllNm phnNmbr =
  mkEmergencyContact
    (accountId contact)
    fllNm
    phnNmbr

emailUpserted :: EmergencyContact -> UTCTime -> Event.EmergencyContactUpserted
emailUpserted contact timestamp =
  Event.EmergencyContactUpserted
    { Event.accountId = unwrapAccountId (accountId contact),
      Event.contactName = unwrapFullName (name contact),
      Event.contactPhone = unwrapPhoneNumber (phoneNumber contact),
      Event.updatedAt = timestamp
    }
